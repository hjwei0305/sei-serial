package com.changhong.sei.serial.sdk;

import com.alibaba.fastjson.JSON;
import com.changhong.sei.serial.sdk.entity.CycleStrategy;
import com.changhong.sei.serial.sdk.entity.SerialConfig;
import com.chonghong.sei.util.thread.ThreadLocalUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.RedisTemplate;

import javax.persistence.Table;
import javax.sql.DataSource;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.annotation.Annotation;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.NumberFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAdjusters;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SerialService {

    private static final Logger log = LoggerFactory.getLogger(SerialService.class);

    private String configAddress;

    private static final String serialUri = "/serialNumberConfig/findByClassName";

    private static final String HEADER_TOKEN_KEY = "x-authorization";

    private static final String SEI_CONFIG_VALUE_REDIS_KEY = "sei-serial:value:";

    private static final Pattern paramPattern = Pattern.compile("(?<=\\$\\{).*?(?=})");

    private static final Pattern serialPattern = Pattern.compile("(?<=#\\{).*?(?=})");

    private RedisTemplate<String,Long> redisTemplate;

    private DataSource dataSource;

    /***
     * 不依赖redis 和 数据库
     * @param configAddress
     */
    public SerialService(String configAddress){
        this.configAddress = configAddress;
    }

    /***
     * 依赖数据库
     *
     * @param configAddress
     * @param redisTemplate
     * @param dataSource
     */
    public SerialService(String configAddress,RedisTemplate redisTemplate,DataSource dataSource) {
        this.configAddress = configAddress;
        this.redisTemplate = redisTemplate;
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
     * @param classPath
     * @return
     */
    public String getNumber(String classPath) {
        return this.getNumber(classPath,null);
    }

    /**
     * 通过类地址和参数获取编号
     * @param classPath
     * @param param
     * @return
     */
    public String getNumber(String classPath,Map<String, String> param){
       return this.getNumber(classPath,param,null);
    }

    /***
     * 通过类地址，参数，表名获取编号
     * @param classPath
     * @param param
     * @param tableName
     * @return
     */
    public String getNumber(String classPath,Map<String, String> param, String tableName){
        SerialConfig config = getSerialConfig(classPath);
        if(Objects.isNull(config)){
            log.error("未获取到相应class【{}】的配置",classPath);
            return null;
        }
        if (Boolean.TRUE.equals(config.getGenFlag())) {
            log.info("直接从服务获取编号进行解析");
            return parserExpression(config, config.getCurrentSerial(),param);
        }
        Long number = getNextNumber(classPath, tableName, config);
        log.info("获得 {} 的下一编号为 {}",classPath,number);
        return parserExpression(config,number,param);
    }

    /**
     * 通过实体类获取标号
     * @param clz
     * @return
     */
    public String getNumber(Class clz){
        return this.getNumber(clz,null);
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
            if(currentAnnotation instanceof Table){
                tableName = ((Table) currentAnnotation).name();
            }
        }
        return getNumber(path ,param, tableName);
    }

    private SerialConfig getSerialConfig(String path) {
        SerialConfig config = null;
        Map<String, String> params = new HashMap<>();
        params.put("className", path);
        try {
            String auth = ThreadLocalUtil.getTranVar(HEADER_TOKEN_KEY);
            log.info("获取当前登录token为 {}", auth);
            String utils = httpGetSerialConfig(configAddress + serialUri, params, auth);
            config = JSON.parseObject(utils,SerialConfig.class);
            log.info("获取 {} 的编号规则为 {}",path,config);
        } catch (Exception e) {
            log.error("获取编号配置出错",e);
        }
        return config;
    }

    private String httpGetSerialConfig(String url, Map<String, String> params, String auth) {
        StringBuilder result = new StringBuilder();
        String urlName = getRequestUrl(url,params);
        log.info("请求给号服务http地址为：{}",urlName);
        try{
            URL realUrl = new URL(urlName);
            //打开和URL之间的连接
            URLConnection conn = realUrl.openConnection();
            //设置通用的请求属性
            conn.setRequestProperty("accept", "*/*");
            conn.setRequestProperty("connection", "Keep-Alive");
            conn.setRequestProperty("user-agent",
                    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1;SV1)");
            if(StringUtils.isNotBlank(auth)){
                conn.setRequestProperty(HEADER_TOKEN_KEY,auth);
            }
            //建立实际的连接
            conn.connect();
            // 定义 BufferedReader输入流来读取URL的响应
            BufferedReader in = new BufferedReader(new InputStreamReader(conn.getInputStream()));
            String line;
            while ((line = in.readLine()) != null) {
                result.append(line);
            }
        } catch (Exception e) {
            log.error("给号服务发送GET请求出现异常", e);
        }
        return result.toString();
    }

    private Long getNextNumber(String path, String tableName,SerialConfig config) {
        String currentKey = SEI_CONFIG_VALUE_REDIS_KEY+path;
        Long currentNumber = 0L;
        if(Objects.isNull(redisTemplate)){
            Long dbCurrent = getMaxNumberFormDB(tableName,config.getExpressionConfig());
            if(Objects.isNull(dbCurrent)){
                return config.getInitialSerial();
            }else {
                return dbCurrent+1;
            }
        }
        if(Boolean.TRUE.equals(redisTemplate.hasKey(currentKey))){
            currentNumber = redisTemplate.opsForValue().increment(currentKey);
        }else {
            Long dbCurrent = getMaxNumberFormDB(tableName,config.getExpressionConfig());
            // 防止多线程重复获取
            long expire = getExpireByCycleStrategy(config.getCycleStrategy());
            if(Objects.nonNull(dbCurrent)){
                currentNumber = dbCurrent+1;
            }else {
                currentNumber = config.getCurrentSerial();
            }
            if(Boolean.FALSE.equals(redisTemplate.opsForValue().setIfAbsent(currentKey,currentNumber,expire,TimeUnit.MILLISECONDS))){
                currentNumber = redisTemplate.opsForValue().increment(currentKey);
            }
        }
        return currentNumber;
    }

    private Long getExpireByCycleStrategy(CycleStrategy cycleStrategy) {
        switch (cycleStrategy){
            case MAX_CYCLE:
                return -1L;
            case YEAR_CYCLE:{
                LocalDateTime now = LocalDateTime.now();
                int currentYear = now.getYear();
                LocalDateTime endYear = LocalDateTime.of(currentYear,12,31,23,59,59);
                return endYear.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()-System.currentTimeMillis();
            }
            case MONTH_CYCLE:{
                LocalDateTime lastDayOfMonth = LocalDateTime.now().with(TemporalAdjusters.lastDayOfMonth());
                return lastDayOfMonth.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()-System.currentTimeMillis();
            }
            default:return 0L;
        }
    }

    private Long getMaxNumberFormDB(String tableName,String expression) {
        if(log.isDebugEnabled()){
            log.debug("获取到 table 名称为 {}", tableName);
        }
        if(Objects.isNull(dataSource)){
            log.info("没有数据库配置，无法进行校准");
            return null;
        }
        if(StringUtils.isNotBlank(tableName)){
            final String sql = "select max(code) as code from "+tableName;
            log.info("编号校准请求sql为 {}", sql);
            try(ResultSet result = dataSource.getConnection().createStatement().executeQuery(sql)) {
                String currentCode = "";
                if(result.next()){
                    currentCode = result.getString("code");
                }
                log.info("获取到当前数据库编号为 {}", currentCode);
                if(StringUtils.isNotBlank(currentCode)){
                    return getCurrentNumber(currentCode,expression);
                }
                return null;
            } catch (SQLException e) {
                log.error("获取数据库编号sql执行出错",e);
            }
        }
        return null;
    }

    private Long getCurrentNumber(String currentCode,String expression) {
        log.info("需要解析的编号 {} 和 表达式 {}",currentCode,expression);
        Matcher serialMatcher = serialPattern.matcher(expression);
        if(serialMatcher.find()){
            String serialItem = serialMatcher.group(0);
            String temp = currentCode.substring(currentCode.length()-serialItem.length());
            try {
                return Long.parseLong(temp);
            }catch (Exception e){
                log.error("校准当前序号出错",e);
            }

        }
        return 0L;
    }

    private String parserExpression(SerialConfig config, Long currentSerial,Map<String, String> param) {
        String expressionConfig = config.getExpressionConfig();
        if(Objects.nonNull(param) && !param.isEmpty()){
            Matcher paramMatcher = paramPattern.matcher(expressionConfig);
            while (paramMatcher.find()){
                String paramItem = paramMatcher.group(0);
                if(isDateParam(paramItem)){
                    String now  = DateTimeFormatter.ofPattern(paramItem).format(LocalDateTime.now());
                    expressionConfig = expressionConfig.replace("${"+paramItem+"}",now);

                }else {
                    expressionConfig = expressionConfig.replace("${"+paramItem+"}",String.valueOf(param.get(paramItem)));
                }
                paramMatcher = paramPattern.matcher(expressionConfig);
            }
        }

        Matcher serialMatcher = serialPattern.matcher(expressionConfig);
        if(serialMatcher.find()){
            String serialItem = serialMatcher.group(0);
            if(config.getCycleStrategy() == CycleStrategy.MAX_CYCLE && String.valueOf(currentSerial).length()>serialItem.length()){
                currentSerial=1L;
                if(Objects.nonNull(redisTemplate)){
                    redisTemplate.opsForValue().set(SEI_CONFIG_VALUE_REDIS_KEY+config.getEntityClassName(), currentSerial);
                }
            }
            expressionConfig = expressionConfig.replace("#{"+serialItem+"}",addZeroForNumber(currentSerial,serialItem.length()));
        }
        return expressionConfig;
    }

    private boolean isDateParam(String paramItem) {
        return "YYYY".equalsIgnoreCase(paramItem) || "YYYYMM".equalsIgnoreCase(paramItem)
                || "YYYYMMDD".equalsIgnoreCase(paramItem) || "YYYYMMDDHH".equalsIgnoreCase(paramItem)
                || "YYYYMMDDHHmm".equalsIgnoreCase(paramItem) || "YYYYMMDDHHmmss".equalsIgnoreCase(paramItem)
                || "YYYYMMddHHmmssSSS".equalsIgnoreCase(paramItem);
    }

    /**
     * 生成补足前导0的编号
     *
     * @param serial 序号
     * @param length 长度
     * @return 编号
     */
    private String addZeroForNumber(Long serial, int length) {
        NumberFormat numberFormat = NumberFormat.getNumberInstance();
        numberFormat.setGroupingUsed(false);
        numberFormat.setMinimumIntegerDigits(length);
        numberFormat.setMaximumIntegerDigits(length);
        return numberFormat.format(serial);
    }

    /**
     * get方式URL拼接
     *
     * @param url
     * @param map
     * @return
     */
    private static String getRequestUrl(String url, Map<String, String> map) {
        if (map == null || map.size() == 0) {
            return url;
        } else {
            StringBuilder newUrl = new StringBuilder(url);
            if (url.indexOf("?") == -1) {
                newUrl.append("?rd=" + Math.random());
            }

            for (Map.Entry<String, String> item : map.entrySet()) {
                if (StringUtils.isNotBlank(item.getKey().trim())) {
                    try {
                        newUrl.append("&" + item.getKey().trim() + "=" + URLEncoder.encode(item.getValue().trim(), "UTF-8"));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
            return newUrl.toString();
        }
    }

    public static void main(String[] args) {
        String expressiong = "ENV${code}${YYYYMMddHHmmssSSS}#{000000}";
//        Map<String,String> param = new HashMap<>();
//        param.put("code","HX");
        SerialService serialService = new SerialService("http://127.0.0.1:8080/serial-service",null,null);
        expressiong = serialService.getNumber("com.changhong.sei.configcenter.entity.TestEntity");
//        Long num = serialService.getCurrentNumber("ENVHX20200205092108103000003", expressiong);
        System.out.println(expressiong);
    }
}
