package com.changhong.sei.serial.controller;

import com.changhong.sei.core.dto.ResultData;
import com.changhong.sei.core.test.BaseUnitTest;
import com.changhong.sei.serial.entity.SerialNumberConfig;
import com.changhong.sei.serial.entity.enumclass.ConfigType;
import com.changhong.sei.serial.entity.enumclass.CycleStrategy;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

import static org.junit.Assert.*;

public class SerialNumberConfigControllerTest extends BaseUnitTest {

    @Autowired
    private SerialNumberConfigController serialNumberConfigController;

    @Test
    public void save() {
//        SerialNumberConfig config = new SerialNumberConfig();
//        config.setActivated(true);
//        config.setExpressionConfig("BAR${yyyyMMDD}#{000000}");
//        config.setConfigType(ConfigType.BAR_TYPE);
//        config.setGenFlag(true);
//        config.setInitialSerial(10);
//        config.setCycleStrategy(CycleStrategy.MONTH_CYCLE);
//        config.setName("test");
//        config.setEntityClassName("com.changhong.sei.configcenter.entity.TestEntity");
//        ResultData<SerialNumberConfig> result = serialNumberConfigController.save(config);
//        System.out.println(result);
    }

    @Test
    public void findByClassName() {
        SerialNumberConfig result = serialNumberConfigController.findByClassName("com.changhong.sei.configcenter.entity.TestEntity");
        System.out.println(result.getCurrentSerial());
    }
}