package com.changhong.sei.serial.sdk;

import com.changhong.sei.serial.sdk.entity.BarCodeDto;

import java.util.HashMap;
import java.util.Map;

public class BarCodeService {

    private SerialServiceProperty serialServiceProperty;

    public BarCodeService(SerialServiceProperty serialServiceProperty){
        this.serialServiceProperty = serialServiceProperty;
    }

    public String genBarCode(String classPath,String referenceId){
        return this.genBarCode(classPath,new HashMap<>(),SerialUtils.DEFAULT_ISOLATION,referenceId);
    }

    public String genBarCode(Class clz,String referenceId){
        String classPath = clz.getName();
        return this.genBarCode(classPath,new HashMap<>(),SerialUtils.DEFAULT_ISOLATION,referenceId);
    }

    public String genBarCode(Class clz,Map<String,String> params, String isolation, String referenceId){
        String classPath = clz.getName();
        return this.genBarCode(classPath,params,isolation,referenceId);
    }

    public String genBarCode(String classPath,Map<String,String> params, String isolation, String referenceId){
        BarCodeDto barCodeDto = new BarCodeDto();
        barCodeDto.setClassPath(classPath);
        barCodeDto.setExpressionParam(params);
        barCodeDto.setReferenceId(referenceId);
        barCodeDto.setIsolation(isolation);
        return SerialUtils.getBarCodeFromService(serialServiceProperty.getUrl(),barCodeDto);
    }
}
