package com.changhong.sei.serial.controller;

import com.changhong.sei.serial.sdk.SerialService;
import org.springframework.boot.jdbc.DataSourceBuilder;

import javax.sql.DataSource;

/**
 * @author liusonglin
 * @version 1.0
 * @Desc
 * @date 2021/9/6 9:26
 */
public class NewTest {

    public static void main(String[] args) {
        SerialService serialService = new SerialService("http://dsei.changhong.com/api-gateway/sei-serial", null, getDataSource());
        long sta = System.currentTimeMillis();
        System.out.println(serialService.getNumber("com.changhong.soms-v6.entity.BusinessCategory"));
//        System.out.println(serialService.getNumber("com.changhong.sei.configcenter.entity.TestEntity", null));
//        Long num = serialService.getCurrentNumber("ENVHX20200205092108103000003", expressiong);
        System.out.println(System.currentTimeMillis() - sta);
    }

    public static DataSource getDataSource() {
        return DataSourceBuilder
                .create()
                .driverClassName("com.mysql.cj.jdbc.Driver")
                .url("jdbc:mysql://10.4.208.134:3306/soms_v6?characterEncoding=utf8&useSSL=false&serverTimezone=Asia/Shanghai")
                .username("sei")
                .password("123456")
                .build();
    }
}
