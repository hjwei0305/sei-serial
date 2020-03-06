package com.changhong.sei.serial.sdk;

public class BarCodeServiceTest {

    public static void main(String[] args) {
        SerialServiceProperty serialServiceProperty = new SerialServiceProperty();
        serialServiceProperty.setUrl("http://127.0.0.1:8080");
        BarCodeService barCodeService = new BarCodeService(serialServiceProperty);
        String a = barCodeService.genBarCode("com.changhong.eams.entity.Archive","123123");
        System.out.println(a);
    }
}