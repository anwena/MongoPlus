package com.anwen.mongo.toolkit;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class IPAddressUtil {

    // 定义正则表达式
    private static final String MONGODB_URI_REGEX = "mongodb://([^:]+):([^@]+)@";
    private static final String IP_PORT_REGEX = "(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3}):(\\d{2})(\\d{1,3})";

    /**
     * 将输入字符串中的IP地址的第二段和第三段以及端口号的前两位替换为星号
     *
     * @param input 包含IP地址和端口号的字符串
     * @return 替换后的字符串
     */
    public static String maskSensitiveInfo(String input) {
        try {
            // 隐藏账号和密码
            Pattern uriPattern = Pattern.compile(MONGODB_URI_REGEX);
            Matcher uriMatcher = uriPattern.matcher(input);
            StringBuffer uriSb = new StringBuffer();

            while (uriMatcher.find()) {
                // 替换账号和密码为***
                String replacement = "mongodb://***:***@";
                uriMatcher.appendReplacement(uriSb, replacement);
            }
            uriMatcher.appendTail(uriSb);

            // 隐藏IP和端口信息
            String intermediateResult = uriSb.toString();
            Pattern ipPortPattern = Pattern.compile(IP_PORT_REGEX);
            Matcher ipPortMatcher = ipPortPattern.matcher(intermediateResult);
            StringBuffer ipPortSb = new StringBuffer();

            while (ipPortMatcher.find()) {
                // 替换匹配的IP地址的第二段和第三段为*
                // 替换端口号的前两位为*
                String replacement = ipPortMatcher.group(1) + ".*.*." + ipPortMatcher.group(4) + ":**" + ipPortMatcher.group(6);
                ipPortMatcher.appendReplacement(ipPortSb, replacement);
            }
            ipPortMatcher.appendTail(ipPortSb);

            return ipPortSb.toString();
        } catch (Exception e) {
            return input;
        }
    }

}
