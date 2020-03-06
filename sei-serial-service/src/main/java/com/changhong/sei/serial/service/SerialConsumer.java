package com.changhong.sei.serial.service;

import com.changhong.sei.core.mq.MqConsumer;
import com.changhong.sei.core.util.JsonUtils;
import com.changhong.sei.serial.dao.IsolationRecordDao;
import com.changhong.sei.serial.dao.SerialNumberConfigDao;
import com.changhong.sei.serial.entity.IsolationRecord;
import com.changhong.sei.serial.entity.SerialNumberConfig;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SerialConsumer extends MqConsumer {

    private final Logger log = LoggerFactory.getLogger(SerialConsumer.class);

    @Autowired
    private IsolationRecordService service;

    @Override
    public void process(String message) {
        if(StringUtils.isBlank(message)){
            return;
        }
        IsolationRecord entity = JsonUtils.fromJson(message, IsolationRecord.class);
        service.save(entity);
    }
}
