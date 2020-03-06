package com.changhong.sei.serial.sdk;

import java.util.HashMap;
import java.util.Map;

public class SerialServiceTest {

    public static void main(String[] args) {
        String expressiong = "ENV${code}${YYYYMMddHHmmssSSS}#{000000}";
        Map<String,String> param = new HashMap<>();
        param.put("code","HX");
        SerialService serialService = new SerialService("http://127.0.0.1:8080", null, null);
        long sta = System.currentTimeMillis();
        System.out.println(serialService.getNumber("com.changhong.sei.configcenter.entity.TestEntity",param,"10018"));
        System.out.println(serialService.getNumber("com.changhong.sei.configcenter.entity.TestEntity",param));
//        Long num = serialService.getCurrentNumber("ENVHX20200205092108103000003", expressiong);
        System.out.println(System.currentTimeMillis() - sta);
    }
}