package com.changhong.sei.serial.sdk;

import com.changhong.sei.serial.sdk.entity.BarCodeDto;

import java.util.HashMap;
import java.util.Map;

public class BarCodeService {

    private SerialServiceProperty serialServiceProperty;

    public BarCodeService(SerialServiceProperty serialServiceProperty){
        this.serialServiceProperty = serialServiceProperty;
    }

    public String genBarCode(String classPath){
        return this.genBarCode(classPath,new HashMap<>());
    }

    public String genBarCode(String classPath,String referenceId){
        return this.genBarCode(classPath,null,referenceId);
    }

    public String genBarCode(Class clz,String referenceId){
        String classPath = clz.getName();
        return this.genBarCode(classPath,new HashMap<>(),referenceId);
    }

    public String genBarCode(Class clz){
        String classPath = clz.getName();
        return this.genBarCode(classPath,new HashMap<>());
    }

    public String genBarCode(Class clz, Map<String,String> params){
        String classPath = clz.getName();
        return this.genBarCode(classPath,params);
    }

    public String genBarCode(String classPath,Map<String,String> params){
        return this.genBarCode(classPath,params,null);
    }

    public String genBarCode(Class clz, Map<String,String> params, String referenceId){
        String classPath = clz.getName();
        return this.genBarCode(classPath,params,referenceId);
    }

    public String genBarCode(String classPath,Map<String,String> params, String referenceId){
        BarCodeDto barCodeDto = new BarCodeDto();
        barCodeDto.setClassPath(classPath);
        barCodeDto.setExpressionParam(params);
        barCodeDto.setReferenceId(referenceId);
        return SerialUtils.getBarCodeFromService(serialServiceProperty.getUrl(),barCodeDto);
    }

    public static void main(String[] args) {
        SerialServiceProperty serialServiceProperty = new SerialServiceProperty();
        serialServiceProperty.setUrl("http://127.0.0.1:8080/serial-service");
        BarCodeService barCodeService = new BarCodeService(serialServiceProperty);
        String a = barCodeService.genBarCode("com.changhong.sei.configcenter.entity.TestEntity","123123");
        System.out.println(a);
    }
}
