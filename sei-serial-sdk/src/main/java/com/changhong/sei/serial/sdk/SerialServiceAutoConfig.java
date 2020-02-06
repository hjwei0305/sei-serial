package com.changhong.sei.serial.sdk;

import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;

import javax.sql.DataSource;

@Configuration
@ConditionalOnProperty(value = "sei.serial.service.enable",havingValue = "true")
@AutoConfigureAfter(DataSourceAutoConfiguration.class)
public class SerialServiceAutoConfig {

    @Bean
    @ConditionalOnMissingBean
    public SerialServiceProperty serialServiceProperty(){
        return new SerialServiceProperty();
    }

    @Bean
    @ConditionalOnMissingBean
    public SerialService serialService(SerialServiceProperty serialServiceProperty){
        return new SerialService(serialServiceProperty.getUrl());
    }


    @Bean
    @ConditionalOnMissingBean
    @ConditionalOnClass({RedisTemplate.class,DataSource.class})
    @ConditionalOnBean({RedisTemplate.class, DataSource.class})
    public SerialService serialService(SerialServiceProperty serialServiceProperty,RedisTemplate redisTemplate,DataSource dataSource){
        return new SerialService(serialServiceProperty.getUrl(),redisTemplate,dataSource);
    }

    @Bean
    @ConditionalOnMissingBean
    @ConditionalOnClass(DataSource.class)
    @ConditionalOnBean({DataSource.class})
    public SerialService serialService(SerialServiceProperty serialServiceProperty,DataSource dataSource){
        return new SerialService(serialServiceProperty.getUrl(),dataSource);
    }
}
