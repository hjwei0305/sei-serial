package com.changhong.sei.serial.service;

import com.changhong.sei.core.context.ContextUtil;
import com.changhong.sei.core.dao.BaseEntityDao;
import com.changhong.sei.core.dto.ResultData;
import com.changhong.sei.core.mq.MqProducer;
import com.changhong.sei.core.service.BaseEntityService;
import com.changhong.sei.core.service.bo.OperateResult;
import com.changhong.sei.core.service.bo.OperateResultWithData;
import com.changhong.sei.core.util.JsonUtils;
import com.changhong.sei.serial.dao.SerialNumberConfigDao;
import com.changhong.sei.serial.entity.BarCodeAssociate;
import com.changhong.sei.serial.entity.IsolationRecord;
import com.changhong.sei.serial.entity.SerialNumberConfig;
import com.changhong.sei.serial.entity.enumclass.ConfigType;
import com.changhong.sei.serial.sdk.SerialUtils;
import com.changhong.sei.serial.sdk.entity.BarCodeDto;
import io.swagger.annotations.Api;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.*;

/**
 * <strong>实现功能:</strong>
 * <p>编号生成器配置服务逻辑实现</p>
 *
 * @author 刘松林
 */
@Service
@Api("SerialNumberConfigService")
public class SerialNumberConfigService extends BaseEntityService<SerialNumberConfig> {
    private final Logger log = LoggerFactory.getLogger(SerialNumberConfigService.class);

    private static final String SEI_SERIAL_CONFIG_REDIS_KEY = "sei-serial:config:";

    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Autowired
    private SerialNumberConfigDao dao;
    @Autowired
    private MqProducer mqProducer;

    @Autowired
    private BarCodeAssociateService barCodeAssociateService;

    @Autowired
    private IsolationRecordService isolationRecordService;


    @Override
    protected BaseEntityDao<SerialNumberConfig> getDao() {
        return dao;
    }


    /**
     * 通过Id获取实体
     *
     * @param className  类路径标识
     * @param configType 配置类型
     * @return 编号生成器配置
     */
    public IsolationRecord findByClassNameAndConfigType(String className, ConfigType configType, String isolation) {
        String tenantCode = ContextUtil.getTenantCode();
        String currentKey = SEI_SERIAL_CONFIG_REDIS_KEY + className + ":" + configType.name() + ":" + tenantCode;
        SerialNumberConfig entity = JsonUtils.fromJson(stringRedisTemplate.opsForValue().get(currentKey), SerialNumberConfig.class);
        if (Objects.isNull(entity)) {
            entity = dao.findByEntityClassNameAndConfigTypeAndTenantCode(className, configType, tenantCode);
            if (Objects.nonNull(entity)) {
                cacheConfig(currentKey, entity);
            }else {
                return null;
            }
        }
        String dateString = SerialUtils.getDateStringByCycleStrategy(entity.getCycleStrategy().name());
        IsolationRecord isolationRecord = isolationRecordService
                .findByConfigIdAndIsolationCodeAndDateString(entity.getId(),isolation,dateString);
        if (log.isDebugEnabled()) {
            log.debug("通过className:{} ,获取到当前配置是 {}", className, entity);
        }
        if (Boolean.TRUE.equals(entity.getGenFlag())) {
            Long currentNumber = entity.getInitialSerial();
            String currentValueKey = SerialUtils.getValueKey(entity.getEntityClassName(),
                    configType.name(),tenantCode,isolation,dateString);
            if (Boolean.TRUE.equals(stringRedisTemplate.hasKey(currentValueKey))) {
                currentNumber = stringRedisTemplate.opsForValue().increment(currentValueKey);
            } else {
                if(Objects.nonNull(isolationRecord)){
                    currentNumber = isolationRecord.getCurrentNumber();
                }
                if (Boolean.FALSE.equals(stringRedisTemplate.opsForValue().setIfAbsent(currentValueKey, currentNumber.toString()))) {
                    currentNumber = stringRedisTemplate.opsForValue().increment(currentValueKey);
                }
            }
            if(Objects.isNull(isolationRecord)){
                isolationRecord = new IsolationRecord();
                isolationRecord.setIsolationCode(isolation);
                isolationRecord.setCurrentNumber(currentNumber);
                isolationRecord.setDateString(dateString);
                isolationRecord.setConfigId(entity.getId());
            }else {
                isolationRecord.setCurrentNumber(currentNumber);
            }
            String json = JsonUtils.toJson(isolationRecord);
            mqProducer.send(json);
            log.info("{} 获取到当前的序列号是 {}", className, currentNumber);
        }else{
            if(Objects.isNull(isolationRecord)){
                isolationRecord = new IsolationRecord();
                isolationRecord.setIsolationCode(isolation);
                isolationRecord.setCurrentNumber(entity.getInitialSerial());
                isolationRecord.setDateString(dateString);
                isolationRecord.setConfigId(entity.getId());
            }
            String json = JsonUtils.toJson(isolationRecord);
            mqProducer.send(json);
        }
        isolationRecord.setSerialNumberConfig(entity);
        return isolationRecord;
    }

    /**
     * 保存一个编号生成器配置
     *
     * @param serialNumberConfig 编号生成器配置
     * @return 操作结果
     */
    @Transactional
    public OperateResultWithData<SerialNumberConfig> save(SerialNumberConfig serialNumberConfig) {
        OperateResultWithData<SerialNumberConfig> result;
        result = super.save(serialNumberConfig);
        String currentKey = SEI_SERIAL_CONFIG_REDIS_KEY + serialNumberConfig.getEntityClassName() + ":" + serialNumberConfig.getConfigType().name() + ":" + serialNumberConfig.getTenantCode();
        cacheConfig(currentKey, serialNumberConfig);
        return result;
    }

    private void cacheConfig(String key, SerialNumberConfig entity) {
        stringRedisTemplate.opsForValue().set(key, JsonUtils.toJson(entity));
    }

    /**
     * 清除编号生成器配置缓存
     *
     * @param id 编号生成器配置Id
     * @return 操作结果
     */
    private ResultData<Void> clearConfigCache(String id) {
        SerialNumberConfig numberConfig = dao.getOne(id);
        clearConfigCache(numberConfig);
        return ResultData.success(null);
    }

    /**
     * 清除编号生成器配置缓存
     */
    public void clearConfigCache() {
        // 清除带有隔离码的编号生成器配置缓存
        String pattern = SEI_SERIAL_CONFIG_REDIS_KEY + "*";
        Set<String> keys = stringRedisTemplate.keys(pattern);
        if (!CollectionUtils.isEmpty(keys)) {
            stringRedisTemplate.delete(keys);
        }
    }

    /**
     * 清除编号生成器配置缓存
     *
     * @param numberConfig 编号生成器配置
     */
    private void clearConfigCache(SerialNumberConfig numberConfig) {
        if (Objects.nonNull(numberConfig)) {
            stringRedisTemplate.delete( SEI_SERIAL_CONFIG_REDIS_KEY + numberConfig.getEntityClassName()
                    + ":" + numberConfig.getConfigType().name() + ":" + numberConfig.getTenantCode());
            String valueKey = SerialUtils.getValueKey(numberConfig.getEntityClassName(),
                    numberConfig.getConfigType().name(), numberConfig.getTenantCode(), "*", "*");
            Set<String> keys = stringRedisTemplate.keys(valueKey);
            if (!CollectionUtils.isEmpty(keys)) {
                stringRedisTemplate.delete(keys);
            }
        }
    }

    @Override
    protected OperateResult preDelete(String s) {
        clearConfigCache(s);
        isolationRecordService.deleteByConfigId(s);
        return super.preDelete(s);
    }

    public Optional<SerialNumberConfig> activatedConfig(String id) {
        Optional<SerialNumberConfig> entity = dao.findById(id);
        return Optional.of(entity)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .map(c -> {
                    c.setActivated(!c.isActivated());
                    clearConfigCache(c);
                    return dao.save(c);
                });
    }

    public String genNumberAndSaveAssociate(BarCodeDto barCodeDto) {
        IsolationRecord isolationRecord = this.findByClassNameAndConfigType(barCodeDto.getClassPath(), ConfigType.BAR_TYPE, barCodeDto.getIsolation());
        if(Objects.isNull(isolationRecord)){
            log.info("获取barCode的参数为：{}",barCodeDto);
            return "获取不到相应配置";
        }
        SerialNumberConfig config = isolationRecord.getSerialNumberConfig();
        String serialItem = SerialUtils.getSerialItem(config.getExpressionConfig());
        if (Boolean.TRUE.equals(config.getGenFlag())) {
            log.info("直接从服务获取编号进行解析");
            String barCode = SerialUtils.parserExpression(config.getExpressionConfig(), isolationRecord.getCurrentNumber(), serialItem, barCodeDto.getExpressionParam());
            if (StringUtils.isNotBlank(barCodeDto.getReferenceId())) {
                BarCodeAssociate barCodeAssociate = new BarCodeAssociate();
                barCodeAssociate.setBarCode(barCode);
                barCodeAssociate.setConfigId(config.getId());
                barCodeAssociate.setReferenceId(barCodeDto.getReferenceId());
                barCodeAssociate.setIsolationCode(barCodeDto.getIsolation());
                barCodeAssociateService.save(barCodeAssociate);
            }
            return barCode;
        }
        return null;
    }
}
