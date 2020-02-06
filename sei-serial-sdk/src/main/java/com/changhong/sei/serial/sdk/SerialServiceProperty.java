package com.changhong.sei.serial.sdk;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties("sei.serial.service")
public class SerialServiceProperty {

    public SerialServiceProperty(){

    }

    private String url;

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }
}
