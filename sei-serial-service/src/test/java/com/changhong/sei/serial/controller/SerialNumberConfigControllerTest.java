package com.changhong.sei.serial.controller;

import com.changhong.com.sei.core.test.BaseUnitTest;
import com.changhong.sei.core.dto.ResultData;
import com.changhong.sei.serial.entity.SerialNumberConfig;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

import static org.junit.Assert.*;

public class SerialNumberConfigControllerTest extends BaseUnitTest {

    @Autowired
    private SerialNumberConfigController serialNumberConfigController;

    @Test
    public void findAll() {
        ResultData<List<SerialNumberConfig>> result = serialNumberConfigController.findAll();
        System.out.println(result);
    }

    @Test
    public void findByClassName() {
        SerialNumberConfig result = serialNumberConfigController.findByClassName("com.changhong.sei.configcenter.entity.TestEntity");
        System.out.println(result.getCurrentSerial());
    }
}