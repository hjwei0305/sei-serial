package com.changhong.sei.serial;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.data.redis.RedisAutoConfiguration;

/**
 * <strong>实现功能:</strong>
 * <p>REST服务主程序</p>
 *
 * @author 王锦光 wangj
 * @version 1.0.1 2019-12-18 10:41
 */
@SpringBootApplication(exclude = {RedisAutoConfiguration.class})
public class SerialRestApplication {
    public static void main(String[] args) {
        SpringApplication.run(SerialRestApplication.class, args);
    }
}
