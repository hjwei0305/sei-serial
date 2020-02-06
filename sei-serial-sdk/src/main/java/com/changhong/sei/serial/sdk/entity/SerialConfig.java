package com.changhong.sei.serial.sdk.entity;

import com.changhong.sei.serial.sdk.entity.CycleStrategy;

public class SerialConfig {
    /**
     * 实体类名（全名）
     */
    private String entityClassName;
    /**
     * 实体名称
     */
    private String name;
    /**
     * 编号表达式
     */
    private String expressionConfig;

    /**
     * 初始序号
     */
    private Long initialSerial;

    /**
     * 当前序号
     */
    private Long currentSerial;

    /**
     * 是否在服务端生成编号
     */
    private Boolean genFlag = Boolean.FALSE;

    private CycleStrategy cycleStrategy = CycleStrategy.MAX_CYCLE;

    public String getEntityClassName() {
        return entityClassName;
    }

    public void setEntityClassName(String entityClassName) {
        this.entityClassName = entityClassName;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getExpressionConfig() {
        return expressionConfig;
    }

    public void setExpressionConfig(String expressionConfig) {
        this.expressionConfig = expressionConfig;
    }

    public Long getInitialSerial() {
        return initialSerial;
    }

    public void setInitialSerial(Long initialSerial) {
        this.initialSerial = initialSerial;
    }

    public Long getCurrentSerial() {
        return currentSerial;
    }

    public void setCurrentSerial(Long currentSerial) {
        this.currentSerial = currentSerial;
    }

    public Boolean getGenFlag() {
        return genFlag;
    }

    public void setGenFlag(Boolean genFlag) {
        this.genFlag = genFlag;
    }

    public CycleStrategy getCycleStrategy() {
        return cycleStrategy;
    }

    public void setCycleStrategy(CycleStrategy cycleStrategy) {
        this.cycleStrategy = cycleStrategy;
    }

    @Override
    public String toString() {
        return "SerialConfig{" +
                "entityClassName='" + entityClassName + '\'' +
                ", name='" + name + '\'' +
                ", expressionConfig='" + expressionConfig + '\'' +
                ", initialSerial=" + initialSerial +
                ", currentSerial=" + currentSerial +
                ", genFlag=" + genFlag +
                ", cycleStrategy=" + cycleStrategy +
                '}';
    }
}
