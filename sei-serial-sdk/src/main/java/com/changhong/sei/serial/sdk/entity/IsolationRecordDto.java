package com.changhong.sei.serial.sdk.entity;


import javax.persistence.*;

public class IsolationRecordDto {

    private String configId;

    private String isolationCode;

    private String dateString;

    private Long currentNumber;

    private SerialConfig serialNumberConfig;

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

    public SerialConfig getSerialNumberConfig() {
        return serialNumberConfig;
    }

    public void setSerialNumberConfig(SerialConfig serialNumberConfig) {
        this.serialNumberConfig = serialNumberConfig;
    }

    @Override
    public String toString() {
        return "IsolationRecordDto{" +
                "configId='" + configId + '\'' +
                ", isolationCode='" + isolationCode + '\'' +
                ", dateString='" + dateString + '\'' +
                ", currentNumber=" + currentNumber +
                ", serialConfig=" + serialNumberConfig +
                '}';
    }
}
