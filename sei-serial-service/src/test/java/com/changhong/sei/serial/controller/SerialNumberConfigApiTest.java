package com.changhong.sei.serial.controller;

import com.changhong.sei.core.dto.ResultData;
import com.changhong.sei.core.test.BaseUnitTest;
import com.changhong.sei.serial.dto.BarCodeAssociateDto;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

public class SerialNumberConfigApiTest extends BaseUnitTest {
    @Autowired
    private SerialNumberConfigController serialNumberConfigController;

    @Test
    public void getReferenceIdByBarCode() {
//        ResultData<BarCodeAssociateDto> result = serialNumberConfigController.getReferenceIdByBarCode("RCS202003000003");
//        System.out.println(result);
    }

    @Test
    public void getBarCodeListByReferenceId() {
//        ResultData<List<BarCodeAssociateDto>> result = serialNumberConfigController.getBarCodeListByReferenceId("123123");
//        System.out.println(result);
    }

    @Test
    public void getReferenceIdsByListBarCode() {
//        List<String> barCodes = new ArrayList<>();
//        barCodes.add("RCS202003000003");
//        barCodes.add("RCS202003000002");
//        ResultData<List<BarCodeAssociateDto>> result = serialNumberConfigController.getReferenceIdsByListBarCode(barCodes);
//        System.out.println(result);
    }
}