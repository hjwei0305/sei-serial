package com.changhong.sei.serial.service;


import com.changhong.sei.core.dao.BaseEntityDao;
import com.changhong.sei.core.service.BaseEntityService;
import com.changhong.sei.core.service.bo.OperateResultWithData;
import com.changhong.sei.core.util.JsonUtils;
import com.changhong.sei.serial.dao.IsolationRecordDao;
import com.changhong.sei.serial.entity.IsolationRecord;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.Objects;
import java.util.Set;

@Service
public class IsolationRecordService extends BaseEntityService<IsolationRecord> {

    private static final String SEI_SERIAL_ISOLATION_REDIS_KEY = "sei-serial:isolation:";

    @Autowired
    private IsolationRecordDao isolationRecordDao;

    @Autowired
    private StringRedisTemplate stringRedisTemplate;


    @Override
    protected BaseEntityDao<IsolationRecord> getDao() {
        return this.isolationRecordDao;
    }

    public IsolationRecord findByConfigIdAndIsolationCodeAndDateString(String configId, String isolation, String dateString) {
        IsolationRecord isolationRecord = getRecord(configId, isolation, dateString);
        if (Objects.isNull(isolationRecord)) {
            isolationRecord = isolationRecordDao.findByConfigIdAndIsolationCodeAndDateString(configId, isolation, dateString);
            if (Objects.nonNull(isolationRecord)) {
                cacheRecord(isolationRecord);
            }
        }
        return isolationRecord;
    }

    @Override
    public OperateResultWithData<IsolationRecord> save(IsolationRecord entity) {
        if (StringUtils.isBlank(entity.getId())) {
            OperateResultWithData<IsolationRecord> result = super.save(entity);
            if (Boolean.TRUE.equals(result.getSuccess())) {
                cacheRecord(result.getData());
            }
            return result;
        } else {
            isolationRecordDao.updateCurrentSerial(entity.getId(), entity.getCurrentNumber());
            cacheRecord(entity);
        }
        return OperateResultWithData.operationSuccessWithData(entity);
    }

    private void cacheRecord(IsolationRecord record) {
        String key = getCacheKey(record.getConfigId(), record.getIsolationCode(), record.getDateString());
        stringRedisTemplate.opsForValue().set(key, JsonUtils.toJson(record));
    }

    private void clearCacheRecord(String configId) {
        String valueKey = getCacheKey(configId, "*", "*");
        Set<String> keys = stringRedisTemplate.keys(valueKey);
        if (!CollectionUtils.isEmpty(keys)) {
            stringRedisTemplate.delete(keys);
        }
    }

    private IsolationRecord getRecord(String configId, String isolation, String dateString) {
        return JsonUtils.fromJson(stringRedisTemplate.opsForValue().get(getCacheKey(configId, isolation, dateString)), IsolationRecord.class);
    }

    private String getCacheKey(String configId, String isolation, String dateString) {
        return SEI_SERIAL_ISOLATION_REDIS_KEY + configId + ":" + isolation + ":" + dateString;
    }

    public void deleteByConfigId(String s) {
        clearCacheRecord(s);
        isolationRecordDao.deleteByConfigId(s);
    }
}
