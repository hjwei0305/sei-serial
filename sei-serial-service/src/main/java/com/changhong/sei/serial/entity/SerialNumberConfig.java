package com.changhong.sei.serial.entity;


import com.changhong.sei.core.entity.BaseAuditableEntity;
import com.changhong.sei.core.entity.ITenant;
import com.changhong.sei.serial.entity.enumclass.ConfigType;
import com.changhong.sei.serial.entity.enumclass.CycleStrategy;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.*;
import javax.validation.constraints.NotNull;

/**
 * <strong>实现功能:</strong>
 * <p>编号生成器配置</p>
 *
 * @author 王锦光 wangj
 * @version 1.0.1 2017-10-19 17:30
 */
@Access(AccessType.FIELD)
@Entity
@Table(name = "serial_number_config")
@DynamicInsert
@DynamicUpdate
public class SerialNumberConfig extends BaseAuditableEntity implements ITenant {
    /**
     * 实体类名（全名）
     */
    @Column(name = "entity_class_name", length = 128, nullable = false)
    private String entityClassName;

    /**
     * 编号类型
     */
    @Column(name = "config_type", length = 32, nullable = false)
    @Enumerated(EnumType.STRING)
    private ConfigType configType = ConfigType.CODE_TYPE;
    /**
     * 实体名称
     */
    @Column(name = "name", length = 32, nullable = false)
    private String name;
    /**
     * 编号表达式
     */
    @Column(name = "expression_config", length = 32)
    private String expressionConfig;

    /**
     * 初始序号
     */
    @Column(name = "initial_serial", nullable = false)
    private long initialSerial;

    /**
     * 是否在服务端生成编号
     */
    @Column(name = "gen_flag")
    private Boolean genFlag = Boolean.FALSE;

    @Column(name = "cycle_strategy")
    private CycleStrategy cycleStrategy = CycleStrategy.MAX_CYCLE;

    @NotNull
    @Column(nullable = false)
    private boolean activated = Boolean.TRUE;

    @Column(name = "tenant_code")
    private String tenantCode;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getEntityClassName() {
        return entityClassName;
    }

    public void setEntityClassName(String entityClassName) {
        this.entityClassName = entityClassName;
    }

    public ConfigType getConfigType() {
        return configType;
    }

    public void setConfigType(ConfigType configType) {
        this.configType = configType;
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

    public long getInitialSerial() {
        return initialSerial;
    }

    public void setInitialSerial(long initialSerial) {
        this.initialSerial = initialSerial;
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

    public boolean isActivated() {
        return activated;
    }

    public void setActivated(boolean activated) {
        this.activated = activated;
    }


    @Override
    public String getTenantCode() {
        return this.tenantCode;
    }

    @Override
    public void setTenantCode(String tenantCode) {
        this.tenantCode = tenantCode;
    }

    @Override
    public String toString() {
        return "SerialNumberConfig{" +
                "entityClassName='" + entityClassName + '\'' +
                ", configType=" + configType +
                ", name='" + name + '\'' +
                ", expressionConfig='" + expressionConfig + '\'' +
                ", initialSerial=" + initialSerial +
                ", genFlag=" + genFlag +
                ", cycleStrategy=" + cycleStrategy +
                ", activated=" + activated +
                ", tenantCode='" + tenantCode + '\'' +
                '}';
    }
}
