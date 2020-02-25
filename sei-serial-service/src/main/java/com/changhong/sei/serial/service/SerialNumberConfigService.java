package com.changhong.sei.serial.service;

import com.changhong.sei.core.context.ContextUtil;
import com.changhong.sei.core.dao.BaseEntityDao;
import com.changhong.sei.core.dto.ResultData;
import com.changhong.sei.core.mq.MqProducer;
import com.changhong.sei.core.service.BaseEntityService;
import com.changhong.sei.core.service.bo.OperateResult;
import com.changhong.sei.core.service.bo.OperateResultWithData;
import com.changhong.sei.core.util.JsonUtils;
import com.changhong.sei.core.util.JwtTokenUtil;
import com.changhong.sei.serial.dao.SerialNumberConfigDao;
import com.changhong.sei.serial.entity.BarCodeAssociate;
import com.changhong.sei.serial.entity.SerialNumberConfig;
import com.changhong.sei.serial.entity.dto.BarCodeDto;
import com.changhong.sei.serial.entity.enumclass.ConfigType;
import com.changhong.sei.serial.entity.enumclass.CycleStrategy;
import com.changhong.sei.serial.sdk.SerialUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.TemporalAdjusters;
import java.util.*;
import java.util.concurrent.TimeUnit;

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

    private static final String SEI_SERIAL_VALUE_REDIS_KEY = "sei-serial:value:";

    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Autowired
    private SerialNumberConfigDao dao;
    @Autowired
    private MqProducer mqProducer;

    @Autowired
    private BarCodeAssociateService barCodeAssociateService;

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
    public SerialNumberConfig findByClassNameAndConfigType(String className, ConfigType configType) {

        String tenantCode = ContextUtil.getTenantCode();
        String currentKey = SEI_SERIAL_CONFIG_REDIS_KEY + className + ":" + configType.name() + tenantCode;
        SerialNumberConfig entity = JsonUtils.fromJson(stringRedisTemplate.opsForValue().get(currentKey), SerialNumberConfig.class);
        if (Objects.isNull(entity)) {
            entity = dao.findByEntityClassNameAndConfigTypeAndTenantCode(className, configType, tenantCode);
            if (Objects.nonNull(entity)) {
                cacheConfig(currentKey, entity);
            }
        }
        if (log.isDebugEnabled()) {
            log.debug("通过className:{} ,获取到当前配置是 {}", className, entity);
        }
        if (Objects.nonNull(entity) && Boolean.TRUE.equals(entity.getGenFlag())) {
            Long currentNumber = entity.getCurrentSerial();
            if (currentNumber < entity.getInitialSerial()) {
                currentNumber = entity.getInitialSerial();
            }
            String currentValueKey = SEI_SERIAL_VALUE_REDIS_KEY + entity.getEntityClassName() + ":" + configType.name() + tenantCode;
            if (Boolean.TRUE.equals(stringRedisTemplate.hasKey(currentValueKey))) {
                currentNumber = stringRedisTemplate.opsForValue().increment(currentValueKey);
            } else {
                long expire = getExpireByCycleStrategy(entity.getCycleStrategy());
                if (Boolean.FALSE.equals(stringRedisTemplate.opsForValue().setIfAbsent(currentValueKey, currentNumber.toString(), expire, TimeUnit.MILLISECONDS))) {
                    currentNumber = stringRedisTemplate.opsForValue().increment(currentValueKey);
                }
            }
            if (currentNumber != null && currentNumber != entity.getCurrentSerial()) {
                entity.setCurrentSerial(currentNumber);
                String json = JsonUtils.toJson(entity);
                cacheConfig(currentKey, entity);
                mqProducer.send(json);
            }
            log.info("{} 获取到当前的序列号是 {}", className, currentNumber);
        }
        return entity;
    }

    @Override
    protected OperateResultWithData<SerialNumberConfig> preInsert(SerialNumberConfig entity) {
        entity.setCurrentSerial(entity.getInitialSerial());
        return super.preInsert(entity);
    }

    private Long getExpireByCycleStrategy(CycleStrategy cycleStrategy) {
        switch (cycleStrategy) {
            case MAX_CYCLE:
                return -1L;
            case YEAR_CYCLE: {
                LocalDateTime now = LocalDateTime.now();
                int currentYear = now.getYear();
                LocalDateTime endYear = LocalDateTime.of(currentYear, 12, 31, 23, 59, 59);
                return endYear.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli() - System.currentTimeMillis();
            }
            case MONTH_CYCLE: {
                LocalDateTime lastDayOfMonth = LocalDateTime.now().with(TemporalAdjusters.lastDayOfMonth());
                return lastDayOfMonth.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli() - System.currentTimeMillis();
            }
            default:
                return 0L;
        }
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
        if (StringUtils.isBlank(serialNumberConfig.getId())) {
            result = super.save(serialNumberConfig);
        } else {
            result = super.save(serialNumberConfig);
        }
        String currentKey = SEI_SERIAL_CONFIG_REDIS_KEY + serialNumberConfig.getEntityClassName() + ":" + serialNumberConfig.getConfigType().name() + serialNumberConfig.getTenantCode();
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
    @ApiOperation("清除编号生成器配置缓存")
    public ResultData<Void> clearConfigCache(String id) {
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
            stringRedisTemplate.delete(SEI_SERIAL_CONFIG_REDIS_KEY + numberConfig.getEntityClassName() + ":" + numberConfig.getConfigType().name() + numberConfig.getTenantCode());
            stringRedisTemplate.delete(SEI_SERIAL_VALUE_REDIS_KEY + numberConfig.getEntityClassName() + ":" + numberConfig.getConfigType().name() + numberConfig.getTenantCode());
        }
    }

    @Override
    protected OperateResult preDelete(String s) {
        clearConfigCache(s);
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
        SerialNumberConfig config = this.findByClassNameAndConfigType(barCodeDto.getClassPath(), ConfigType.BAR_TYPE);
        String serialItem = SerialUtils.getSerialItem(config.getExpressionConfig());
        if (Boolean.TRUE.equals(config.getGenFlag())) {
            log.info("直接从服务获取编号进行解析");
            String barCode = SerialUtils.parserExpression(config.getExpressionConfig(), config.getCurrentSerial(), serialItem, barCodeDto.getExpressionParam());
            if (StringUtils.isNotBlank(barCodeDto.getReferenceId())) {
                BarCodeAssociate barCodeAssociate = new BarCodeAssociate();
                barCodeAssociate.setBarCode(barCode);
                barCodeAssociate.setConfigId(config.getId());
                barCodeAssociate.setReferenceId(barCodeDto.getReferenceId());
                barCodeAssociateService.save(barCodeAssociate);
            }
            return barCode;
        }
        return null;
    }

    public static void main(String[] args) {
        JwtTokenUtil jwtTokenUtil = new JwtTokenUtil();
        String token = "eyJhbGciOiJIUzUxMiJ9.eyJyYW5kb21LZXkiOiIwREI3QkY2Ni01NEFFLTExRUEtOTEyNS0wODAwNTgwMDAwMDUiLCJzdWIiOiJhZG1pbiIsImF1dGhvcml0eVBvbGljeSI6IlRlbmFudEFkbWluIiwiaXAiOiJVbmtub3duIiwidXNlclR5cGUiOiJFbXBsb3llZSIsInVzZXJOYW1lIjoi57O757uf566h55CG5ZGYIiwibG9jYWxlIjoiemhfQ04iLCJleHAiOjE1ODIzNzgxMTEsInVzZXJJZCI6IkI1NEU4OTY0LUQxNEQtMTFFOC1BNjRCLTAyNDJDMEE4NDQxQiIsImlhdCI6MTU4MjI5MTcxMSwidGVuYW50IjoiMTAwNDQiLCJhY2NvdW50IjoiYWRtaW4ifQ.h1EC_5IoZt-92jI2h1zH8hsxbk5h94ewVxSR7IgMI67oIFsAtvx3GOcXpzffv2gnjDBxfJqfECTAAFX4Vpji3w";

        String s = jwtTokenUtil.getSubjectFromToken(token);
        System.out.println(s);
    }
}
