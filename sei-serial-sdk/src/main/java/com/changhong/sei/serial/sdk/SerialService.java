package com.changhong.sei.serial.sdk;

import com.changhong.sei.serial.sdk.entity.ConfigType;
import com.changhong.sei.serial.sdk.entity.CycleStrategy;
import com.changhong.sei.serial.sdk.entity.SerialConfig;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.StringRedisTemplate;

import javax.persistence.Table;
import javax.sql.DataSource;
import java.lang.annotation.Annotation;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.TemporalAdjusters;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

public class SerialService {

    private static final Logger log = LoggerFactory.getLogger(SerialService.class);

    private String configAddress;

    private static final String SEI_CONFIG_VALUE_REDIS_KEY = "sei-serial:value:";

    private StringRedisTemplate stringRedisTemplate;

    private DataSource dataSource;

    /***
     * 不依赖redis 和 数据库
     * @param configAddress
     */
    public SerialService(String configAddress) {
        this.configAddress = configAddress;
    }

    /***
     * 依赖数据库
     *
     * @param configAddress
     * @param stringRedisTemplate
     * @param dataSource
     */
    public SerialService(String configAddress, StringRedisTemplate stringRedisTemplate, DataSource dataSource) {
        this.configAddress = configAddress;
        this.stringRedisTemplate = stringRedisTemplate;
        this.dataSource = dataSource;
    }

    /***
     * 依赖数据库，不依赖redis
     *
     * @param url
     * @param dataSource
     */
    public SerialService(String url, DataSource dataSource) {
        this.configAddress = url;
        this.dataSource = dataSource;
    }

    /**
     * 通过类地址获取编号
     *
     * @param classPath
     * @return
     */
    public String getNumber(String classPath) {
        return this.getNumber(classPath, null);
    }


    /**
     * 通过类地址和参数获取编号
     *
     * @param classPath
     * @param param
     * @return
     */
    public String getNumber(String classPath, Map<String, String> param) {
        return this.getNumber(classPath, param, null);
    }

    /***
     * 通过类地址，参数，表名获取编号
     * @param classPath
     * @param param
     * @param tableName
     * @return
     */
    public String getNumber(String classPath, Map<String, String> param, String tableName) {
        SerialConfig config = SerialUtils.getSerialConfig(configAddress, classPath);
        if (Objects.isNull(config)) {
            log.error("未获取到相应class【{}】的配置", classPath);
            return null;
        }
        String serialItem = SerialUtils.getSerialItem(config.getExpressionConfig());
        if (Boolean.TRUE.equals(config.getGenFlag())) {
            log.info("直接从服务获取编号进行解析");
            return SerialUtils.parserExpression(config.getExpressionConfig(), config.getCurrentSerial(), serialItem, param);
        }
        Long number = getNextNumber(classPath, tableName, config);
        number = adjustCurrentNumber(number,config,serialItem);
        log.info("获得 {} 的下一编号为 {}", classPath, number);
        return SerialUtils.parserExpression(config.getExpressionConfig(), number, serialItem, param);
    }

    /**
     * 校准当前序列
     * @param currentSerial
     * @param config
     * @param serialItem
     * @return
     */
    private Long adjustCurrentNumber(Long currentSerial, SerialConfig config, String serialItem){
        if (config.getCycleStrategy() == CycleStrategy.MAX_CYCLE && String.valueOf(currentSerial).length() > serialItem.length()) {
            currentSerial = 1L;
            if (Objects.nonNull(stringRedisTemplate)) {
                String currentKey = SEI_CONFIG_VALUE_REDIS_KEY + config.getEntityClassName() + ":" + ConfigType.CODE_TYPE + config.getTenantCode();
                stringRedisTemplate.opsForValue().set(currentKey, currentSerial.toString());
            }
        }
        return currentSerial;
    }

    /**
     * 通过实体类获取标号
     *
     * @param clz
     * @return
     */
    public String getNumber(Class clz) {
        return this.getNumber(clz, null);
    }


    /***
     * 通过实体类和参数获取编号
     * @param clz
     * @param param
     * @return
     */
    public String getNumber(Class clz, Map<String, String> param) {
        String path = clz.getName();
        Annotation[] annotations = clz.getAnnotations();
        String tableName = "";
        for (int i = 0; i < annotations.length; i++) {
            Annotation currentAnnotation = annotations[i];
            if (currentAnnotation instanceof Table) {
                tableName = ((Table) currentAnnotation).name();
            }
        }
        return getNumber(path, param, tableName);
    }

    private Long getNextNumber(String path, String tableName, SerialConfig config) {
        String currentKey = SEI_CONFIG_VALUE_REDIS_KEY + path + ":" + ConfigType.CODE_TYPE + config.getTenantCode();
        Long currentNumber = 0L;
        if (Objects.isNull(stringRedisTemplate)) {
            Long dbCurrent = getMaxNumberFormDB(tableName, config.getExpressionConfig());
            if (Objects.isNull(dbCurrent)) {
                return config.getInitialSerial();
            } else {
                return dbCurrent + 1;
            }
        }
        if (Boolean.TRUE.equals(stringRedisTemplate.hasKey(currentKey))) {
            currentNumber = stringRedisTemplate.opsForValue().increment(currentKey);
        } else {
            Long dbCurrent = getMaxNumberFormDB(tableName, config.getExpressionConfig());
            // 防止多线程重复获取
            long expire = getExpireByCycleStrategy(config.getCycleStrategy());
            if (Objects.nonNull(dbCurrent)) {
                currentNumber = dbCurrent + 1;
            } else {
                currentNumber = config.getCurrentSerial();
            }
            if (Boolean.FALSE.equals(stringRedisTemplate.opsForValue().setIfAbsent(currentKey, currentNumber.toString(), expire, TimeUnit.MILLISECONDS))) {
                currentNumber = stringRedisTemplate.opsForValue().increment(currentKey);
            }
        }
        return currentNumber;
    }

    private Long getExpireByCycleStrategy(CycleStrategy cycleStrategy) {
        switch (cycleStrategy) {
            case MAX_CYCLE:
                return -1L;
            case YEAR_CYCLE: {
                LocalDateTime now = LocalDateTime.now();
                int currentYear = now.getYear();
                LocalDateTime endYear = LocalDateTime.of(currentYear, 12, 31, 23, 59, 59);
                return endYear.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli() - System.currentTimeMillis();
            }
            case MONTH_CYCLE: {
                LocalDateTime lastDayOfMonth = LocalDateTime.now().with(TemporalAdjusters.lastDayOfMonth());
                return lastDayOfMonth.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli() - System.currentTimeMillis();
            }
            default:
                return 0L;
        }
    }

    private Long getMaxNumberFormDB(String tableName, String expression) {
        if (log.isDebugEnabled()) {
            log.debug("获取到 table 名称为 {}", tableName);
        }
        if (Objects.isNull(dataSource)) {
            log.info("没有数据库配置，无法进行校准");
            return null;
        }
        if (StringUtils.isNotBlank(tableName)) {
            final String sql = "select max(code) as code from " + tableName;
            log.info("编号校准请求sql为 {}", sql);
            try (ResultSet result = dataSource.getConnection().createStatement().executeQuery(sql)) {
                String currentCode = "";
                if (result.next()) {
                    currentCode = result.getString("code");
                }
                log.info("获取到当前数据库编号为 {}", currentCode);
                if (StringUtils.isNotBlank(currentCode)) {
                    return SerialUtils.parseExpressionAndGetNumber(currentCode, expression);
                }
                return null;
            } catch (SQLException e) {
                log.error("获取数据库编号sql执行出错", e);
            }
        }
        return null;
    }


    public static void main(String[] args) {
        String expressiong = "ENV${code}${YYYYMMddHHmmssSSS}#{000000}";
//        Map<String,String> param = new HashMap<>();
//        param.put("code","HX");
        SerialService serialService = new SerialService("http://127.0.0.1:8080/serial-service", null, null);
        long sta = System.currentTimeMillis();
        expressiong = serialService.getNumber("com.changhong.sei.configcenter.entity.TestEntity");
//        Long num = serialService.getCurrentNumber("ENVHX20200205092108103000003", expressiong);
        System.out.println(expressiong);
        System.out.println(System.currentTimeMillis() - sta);
    }
}
