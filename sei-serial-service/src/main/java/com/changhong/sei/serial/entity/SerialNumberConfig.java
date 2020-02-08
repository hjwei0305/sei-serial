package com.changhong.sei.serial.entity;


import com.changhong.sei.core.entity.BaseEntity;
import com.changhong.sei.serial.entity.enumclass.CycleStrategy;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.Date;

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
public class SerialNumberConfig extends BaseEntity {
    /**
     * 实体类名（全名）
     */
    @Column(name = "entity_class_name", length = 128, nullable = false)
    private String entityClassName;

    /**
     * 隔离码
     */
    @Column(name = "isolation_code", length = 32, nullable = false)
    private String isolationCode;
    /**
     * 实体名称
     */
    @Column(name = "name", length = 32, nullable = false)
    private String name;
    /**
     * 编号前缀
     */
    @Column(name = "expression_config", length = 32)
    private String expressionConfig;

    /**
     * 初始序号
     */
    @Column(name = "initial_serial", nullable = false)
    private long initialSerial;

    /**
     * 当前序号
     */
    @Column(name = "current_serial", nullable = false)
    private long currentSerial;

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


    @Column(name = "create_date")
    @Temporal(TemporalType.TIMESTAMP)
    private Date createDate;

    @Column(name = "create_account")
    private String createAccount;

    @Column(name = "edit_date")
    @Temporal(TemporalType.TIMESTAMP)
    private Date editDate;

    @Column(name = "edit_account")
    private String editAccount;

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

    public String getIsolationCode() {
        return isolationCode;
    }

    public void setIsolationCode(String isolationCode) {
        this.isolationCode = isolationCode;
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

    public long getCurrentSerial() {
        return currentSerial;
    }

    public void setCurrentSerial(long currentSerial) {
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

    public boolean isActivated() {
        return activated;
    }

    public void setActivated(boolean activated) {
        this.activated = activated;
    }


    public String getCreateAccount() {
        return createAccount;
    }

    public void setCreateAccount(String createAccount) {
        this.createAccount = createAccount;
    }


    public Date getCreateDate() {
        return createDate;
    }

    public void setCreateDate(Date createDate) {
        this.createDate = createDate;
    }

    public Date getEditDate() {
        return editDate;
    }

    public void setEditDate(Date editDate) {
        this.editDate = editDate;
    }

    public String getEditAccount() {
        return editAccount;
    }

    public void setEditAccount(String editAccount) {
        this.editAccount = editAccount;
    }
}
