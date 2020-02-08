package com.changhong.sei.serial.service;

import com.changhong.sei.core.mq.MqConsumer;
import com.changhong.sei.core.util.JsonUtils;
import com.changhong.sei.serial.dao.SerialNumberConfigDao;
import com.changhong.sei.serial.entity.SerialNumberConfig;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SerialConsumer extends MqConsumer {

    private final Logger log = LoggerFactory.getLogger(SerialNumberConfigService.class);

    @Autowired
    private SerialNumberConfigDao dao;

    @Override
    public void process(String message) {
        if(StringUtils.isBlank(message)){
            return;
        }
        if(log.isDebugEnabled()){
            log.debug("接收到的消息为：{}",message);
        }
        SerialNumberConfig entity = JsonUtils.fromJson(message, SerialNumberConfig.class);
        dao.updateCurrentSerial(entity.getId(),entity.getCurrentSerial());
    }
}
