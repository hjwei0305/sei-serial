package com.changhong.sei.serial.sdk;

import com.alibaba.fastjson.JSON;
import com.changhong.sei.serial.sdk.entity.BarCodeDto;
import com.changhong.sei.serial.sdk.entity.ConfigType;
import com.changhong.sei.serial.sdk.entity.CycleStrategy;
import com.changhong.sei.serial.sdk.entity.SerialConfig;
import com.chonghong.sei.util.thread.ThreadLocalUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.text.NumberFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SerialUtils {

    private SerialUtils() {
    }

    private static final Logger log = LoggerFactory.getLogger(SerialUtils.class);

    private static final String SERIAL_URI = "/serialNumberConfig/findByClassNameAndType";

    private static final String BARCODE_URI = "/serialNumberConfig/genAndSaveAssociate";

    private static final Pattern paramPattern = Pattern.compile("(?<=\\$\\{).*?(?=})");

    private static final Pattern serialPattern = Pattern.compile("(?<=#\\{).*?(?=})");

    private static final String HEADER_TOKEN_KEY = "x-authorization";

    public static SerialConfig getSerialConfig(String configAddress, String path) {
        SerialConfig config = null;
        Map<String, String> params = new HashMap<>();
        params.put("className", path);
        try {
            String urlName = getRequestUrl(configAddress + SERIAL_URI, params);
            log.info("请求给号服务http地址为：{}", urlName);
            String utils = getHttpResult(urlName, "GET", null);
            config = JSON.parseObject(utils, SerialConfig.class);
            log.info("获取 {} 的编号规则为 {}", path, config);
        } catch (Exception e) {
            log.error("获取编号配置出错", e);
        }
        return config;
    }

    public static String getBarCodeFromService(String configAddress, BarCodeDto barCodeDto) {
        String urlName = configAddress + BARCODE_URI;
        log.info("请求给号服务http地址为：{}", urlName);
        return getHttpResult(urlName, "POST", barCodeDto);
    }

    private static String getHttpResult(String url, String method, Object params) {
        StringBuilder result = new StringBuilder();
        try {
            HttpURLConnection conn = getConnection(url, method, params);
            if (Objects.isNull(conn)) {
                return null;
            }
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

    private static HttpURLConnection getConnection(String urlName, String method, Object params) {
        try {
            URL realUrl = new URL(urlName);
            //打开和URL之间的连接
            HttpURLConnection conn = (HttpURLConnection) realUrl.openConnection();
            //设置通用的请求属性
            conn.setRequestProperty("Content-Type", "application/json");
            conn.setRequestProperty("Accept", "application/json");
            conn.setRequestProperty("connection", "Keep-Alive");
            conn.setRequestProperty("user-agent",
                    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1;SV1)");
            conn.setDoInput(true);    //true表示允许获得输入流,读取服务器响应的数据,该属性默认值为true
            conn.setDoOutput(true);   //true表示允许获得输出流,向远程服务器发送数据,该属性默认值为false
            conn.setUseCaches(false); //禁止缓存
            conn.setReadTimeout(10000);    //30秒读取超时
            conn.setConnectTimeout(10000);
            conn.setRequestMethod(method);

            //获取输出流
            if (Objects.nonNull(params)) {
                OutputStreamWriter out = null;
                out = new OutputStreamWriter(conn.getOutputStream());
                String jsonStr = JSON.toJSONString(params);
                out.write(jsonStr);
                out.flush();
                out.close();
            }
            String auth = ThreadLocalUtil.getTranVar(HEADER_TOKEN_KEY);
            log.info("获取当前登录token为 {}", auth);
            if (StringUtils.isNotBlank(auth)) {
                conn.setRequestProperty(HEADER_TOKEN_KEY, auth);
            }
            //建立实际的连接
            conn.connect();
            return conn;
        } catch (Exception e) {
            log.error("建立给号服务请求连接出错", e);
        }
        return null;
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
            if (!url.contains("?")) {
                newUrl.append("?rd=");
                newUrl.append(Math.random());
            }

            for (Map.Entry<String, String> item : map.entrySet()) {
                if (StringUtils.isNotBlank(item.getKey().trim())) {
                    try {
                        if (StringUtils.isNotBlank(item.getValue())) {
                            newUrl.append("&");
                            newUrl.append(item.getKey().trim());
                            newUrl.append("=");
                            newUrl.append(URLEncoder.encode(item.getValue().trim(), "UTF-8"));
                        }
                    } catch (Exception e) {
                        log.error("生成给号配置请求url出错", e);
                    }
                }
            }
            return newUrl.toString();
        }
    }

    public static String parserExpression(String expressionConfig, Long currentSerial, String serialItem, Map<String, String> param) {
        Matcher paramMatcher = paramPattern.matcher(expressionConfig);
        while (paramMatcher.find()) {
            String paramItem = paramMatcher.group(0);
            if (isDateParam(paramItem)) {
                String now = DateTimeFormatter.ofPattern(paramItem).format(LocalDateTime.now());
                expressionConfig = expressionConfig.replace("${" + paramItem + "}", now);

            } else if(!CollectionUtils.isEmpty(param)){
                expressionConfig = expressionConfig.replace("${" + paramItem + "}", String.valueOf(param.get(paramItem)));
            }
            paramMatcher = paramPattern.matcher(expressionConfig);
        }
        expressionConfig = expressionConfig.replace("#{" + serialItem + "}", addZeroForNumber(currentSerial, serialItem.length()));
        return expressionConfig;
    }

    public static String getSerialItem(String expressionConfig) {
        Matcher serialMatcher = serialPattern.matcher(expressionConfig);
        if (serialMatcher.find()) {
            String serialItem = serialMatcher.group(0);
            return serialItem;
        }
        return "0";
    }

    public static Long parseExpressionAndGetNumber(String currentCode, String expression) {
        log.info("需要解析的编号 {} 和 表达式 {}", currentCode, expression);
        Matcher serialMatcher = serialPattern.matcher(expression);
        if (serialMatcher.find()) {
            String serialItem = serialMatcher.group(0);
            String temp = currentCode.substring(currentCode.length() - serialItem.length());
            try {
                return Long.parseLong(temp);
            } catch (Exception e) {
                log.error("校准当前序号出错", e);
            }

        }
        return 0L;
    }

    private static boolean isDateParam(String paramItem) {
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
    private static String addZeroForNumber(Long serial, int length) {
        NumberFormat numberFormat = NumberFormat.getNumberInstance();
        numberFormat.setGroupingUsed(false);
        numberFormat.setMinimumIntegerDigits(length);
        numberFormat.setMaximumIntegerDigits(length);
        return numberFormat.format(serial);
    }
}
