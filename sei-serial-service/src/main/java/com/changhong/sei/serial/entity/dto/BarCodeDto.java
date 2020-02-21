package com.changhong.sei.serial.entity.dto;

import java.io.Serializable;
import java.util.Map;

/**
 * 实现功能：
 *
 * @author  刘松林
 * @version 1.0.00  2020-02-20 17:09
 */
public class BarCodeDto implements Serializable {

    private static final long serialVersionUID = 132979558246200348L;


    /**
     * 类路径
     */
    private String classPath;

    /***
     * 关联id
     */
    private String referenceId;

    /**
     * 表达式参数
     */
    private Map<String,String> expressionParam;

    public String getClassPath() {
        return classPath;
    }

    public void setClassPath(String classPath) {
        this.classPath = classPath;
    }

    public String getReferenceId() {
        return referenceId;
    }

    public void setReferenceId(String referenceId) {
        this.referenceId = referenceId;
    }

    public Map<String, String> getExpressionParam() {
        return expressionParam;
    }

    public void setExpressionParam(Map<String, String> expressionParam) {
        this.expressionParam = expressionParam;
    }
}
