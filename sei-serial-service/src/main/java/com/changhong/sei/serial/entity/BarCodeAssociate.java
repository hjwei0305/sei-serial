package com.changhong.sei.serial.entity;

import com.changhong.sei.core.entity.BaseAuditableEntity;
import com.changhong.sei.core.entity.ITenant;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.*;

@Access(AccessType.FIELD)
@Entity
@Table(name = "bar_code_associate")
@DynamicInsert
@DynamicUpdate
public class BarCodeAssociate extends BaseAuditableEntity implements ITenant {

    @Column(name = "bar_code")
    private String barCode;

    @Column(name = "reference_id")
    private String referenceId;

    @Column(name = "tenant_code")
    private String tenantCode;

    @Column(name = "config_id")
    private String configId;

    public String getBarCode() {
        return barCode;
    }

    public void setBarCode(String barCode) {
        this.barCode = barCode;
    }

    public String getReferenceId() {
        return referenceId;
    }

    public void setReferenceId(String referenceId) {
        this.referenceId = referenceId;
    }

    public String getConfigId() {
        return configId;
    }

    public void setConfigId(String configId) {
        this.configId = configId;
    }


    @Override
    public String getTenantCode() {
        return tenantCode;
    }

    @Override
    public void setTenantCode(String tenantCode) {
        this.tenantCode = tenantCode;
    }
}
