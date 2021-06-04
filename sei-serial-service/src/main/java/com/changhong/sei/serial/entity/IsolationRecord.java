package com.changhong.sei.serial.entity;


import com.changhong.sei.core.entity.BaseAuditableEntity;
import com.changhong.sei.core.entity.ITenant;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.*;

@Access(AccessType.FIELD)
@Entity
@Table(name = "isolation_record")
@DynamicInsert
@DynamicUpdate
public class IsolationRecord extends BaseAuditableEntity implements ITenant {

    @Column(name = "config_id")
    private String configId;

    @Column(name = "isolation_code")
    private String isolationCode;

    @Column(name = "date_string")
    private String dateString;

    @Column(name = "current_number")
    private Long currentNumber;

    @Column(name = "tenant_code")
    private String tenantCode;

    @Transient
    private SerialNumberConfig serialNumberConfig;

    public String getConfigId() {
        return configId;
    }

    public void setConfigId(String configId) {
        this.configId = configId;
    }

    public String getIsolationCode() {
        return isolationCode;
    }

    public void setIsolationCode(String isolationCode) {
        this.isolationCode = isolationCode;
    }

    public String getDateString() {
        return dateString;
    }

    public void setDateString(String dateString) {
        this.dateString = dateString;
    }

    public Long getCurrentNumber() {
        return currentNumber;
    }

    public void setCurrentNumber(Long currentNumber) {
        this.currentNumber = currentNumber;
    }

    public SerialNumberConfig getSerialNumberConfig() {
        return serialNumberConfig;
    }

    public void setSerialNumberConfig(SerialNumberConfig serialNumberConfig) {
        this.serialNumberConfig = serialNumberConfig;
    }

    @Override
    public String toString() {
        return "IsolationRecord{" +
                "configId='" + configId + '\'' +
                ", isolationCode='" + isolationCode + '\'' +
                ", dateString='" + dateString + '\'' +
                ", currentNumber=" + currentNumber +
                ", serialNumberConfig=" + serialNumberConfig +
                '}';
    }

    @Override
    public String getTenantCode() {
        return this.tenantCode;
    }

    @Override
    public void setTenantCode(String tenantCode) {
        this.tenantCode = tenantCode;
    }
}
