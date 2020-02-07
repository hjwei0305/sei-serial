package com.changhong.sei.serial.service;

import com.changhong.sei.core.dto.ResultData;
import com.changhong.sei.core.util.JsonUtils;
import com.changhong.sei.serial.dao.SerialNumberConfigDao;
import com.changhong.sei.serial.entity.SerialNumberConfig;
import com.changhong.sei.serial.entity.enumclass.CycleStrategy;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
 * @author 王锦光 wangj
 * @version 1.0.1 2017-10-24 20:28
 */
@Service
@Api("SerialNumberConfigService")
public class SerialNumberConfigService {
    private final Logger log = LoggerFactory.getLogger(SerialNumberConfigService.class);

    private static final String SEI_SERIAL_CONFIG_REDIS_KEY= "sei-serial:config:";

    private static final String SEI_SERIAL_VALUE_REDIS_KEY = "sei-serial:value:";

    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Autowired
    private SerialNumberConfigDao dao;


    /**
     * 通过Id获取实体
     *
     * @return 编号生成器配置
     */
    @ApiOperation("获取编号生成器配置")
    public List<SerialNumberConfig> findAll() {
        return dao.findAll();
    }

    /**
     * 通过Id获取实体
     *
     * @param className Id标识
     * @return 编号生成器配置
     */
    @ApiOperation("通过Id获取编号生成器配置")
    public SerialNumberConfig findByClassName(String className) {
        String currentKey = SEI_SERIAL_CONFIG_REDIS_KEY + className;
        SerialNumberConfig entity = JsonUtils.fromJson(stringRedisTemplate.opsForValue().get(currentKey),SerialNumberConfig.class);
        if (Objects.isNull(entity)) {
            entity = dao.findByEntityClassName(className);
            if(Objects.nonNull(entity)){
                cacheConfig(entity);
            }
        }
        if(log.isDebugEnabled()){
            log.debug("通过className:{} ,获取到当前配置是 {}", className, entity);
        }
        if(Objects.nonNull(entity) && Boolean.TRUE.equals(entity.getGenFlag())){
            Long currentNumber = entity.getCurrentSerial();
            String currentValueKey = SEI_SERIAL_VALUE_REDIS_KEY+entity.getEntityClassName();
            if(stringRedisTemplate.hasKey(currentValueKey)){
                currentNumber = stringRedisTemplate.opsForValue().increment(currentValueKey);
            }else{
                long expire = getExpireByCycleStrategy(entity.getCycleStrategy());
                if(Boolean.FALSE.equals(stringRedisTemplate.opsForValue().setIfAbsent(currentValueKey,currentNumber.toString(),expire,TimeUnit.MILLISECONDS))){
                    currentNumber = stringRedisTemplate.opsForValue().increment(currentValueKey);
                }
            }
            log.info("{} 获取到当前的序列号是 {}",className ,currentNumber);
            if(currentNumber != entity.getCurrentSerial()){
                dao.updateCurrentSerial(entity.getId(),currentNumber);
                entity.setCurrentSerial(currentNumber);
                cacheConfig(entity);
            }
        }
        return entity;
    }

    private Long getExpireByCycleStrategy(CycleStrategy cycleStrategy) {
        switch (cycleStrategy){
            case MAX_CYCLE:
                return -1L;
            case YEAR_CYCLE:{
                LocalDateTime now = LocalDateTime.now();
                int currentYear = now.getYear();
                LocalDateTime endYear = LocalDateTime.of(currentYear,12,31,23,59,59);
                return endYear.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()-System.currentTimeMillis();
            }
            case MONTH_CYCLE:{
                LocalDateTime lastDayOfMonth = LocalDateTime.now().with(TemporalAdjusters.lastDayOfMonth());
                return lastDayOfMonth.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()-System.currentTimeMillis();
            }
            default:return 0L;
        }
    }

    /**
     * 保存一个编号生成器配置
     *
     * @param serialNumberConfig 编号生成器配置
     * @return 操作结果
     */
    @Transactional
    public SerialNumberConfig save(SerialNumberConfig serialNumberConfig) {
        SerialNumberConfig entity = dao.save(serialNumberConfig);
        cacheConfig(entity);
        return entity;
    }

    private void cacheConfig(SerialNumberConfig entity) {
        stringRedisTemplate.opsForValue().set(SEI_SERIAL_CONFIG_REDIS_KEY + entity.getEntityClassName(),JsonUtils.toJson(entity));
    }

    /**
     * 删除一个编号生成器配置
     *
     * @param id 编号生成器配置Id
     * @return 操作结果
     */
    @ApiOperation("删除一个编号生成器配置")
    public void delete(String id) {
        dao.deleteById(id);
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
        if (!keys.isEmpty()) {
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
            stringRedisTemplate.delete(SEI_SERIAL_CONFIG_REDIS_KEY + numberConfig.getEntityClassName());
        }
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


}