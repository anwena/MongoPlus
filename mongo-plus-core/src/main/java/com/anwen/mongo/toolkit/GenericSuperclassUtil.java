package com.anwen.mongo.toolkit;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author JiaChaoYang
 *  泛型操作
 * @since 2023-02-09 15:02
 **/
public class GenericSuperclassUtil {

    private static final Pattern humpPattern = Pattern.compile("[A-Z]");


    /*
     * 获取泛型类Class对象，不是泛型类则返回null
     */
    public static Class<?> getActualTypeArgument(Class<?> clazz) {
        Class<?> entitiClass = null;
        Type genericSuperclass = clazz.getGenericSuperclass();
        if (genericSuperclass instanceof ParameterizedType) {
            Type[] actualTypeArguments = ((ParameterizedType) genericSuperclass)
                    .getActualTypeArguments();
            if (actualTypeArguments != null && actualTypeArguments.length > 0) {
                entitiClass = (Class<?>) actualTypeArguments[0];
            }
        }

        return entitiClass;
    }

    /**
     * 下划线转驼峰
     * @param str 字符串
     * @return java.lang.String
     * @author JiaChaoYang
     * @since 2023/2/10 10:49
    */
    public static String underlineToHump (String str){
        str = str.substring(3);
        Pattern UNDERLINE_PATTERN = Pattern.compile("_([a-z])");
        //正则匹配下划线及后一个字符，删除下划线并将匹配的字符转成大写
        Matcher matcher = UNDERLINE_PATTERN.matcher(str);
        StringBuffer sb = new StringBuffer(str);
        if (matcher.find()) {
            sb = new StringBuffer();
            //将当前匹配的子串替换成指定字符串，并且将替换后的子串及之前到上次匹配的子串之后的字符串添加到StringBuffer对象中
            //正则之前的字符和被替换的字符
            matcher.appendReplacement(sb, matcher.group(1).toUpperCase());
            //把之后的字符串也添加到StringBuffer对象中
            matcher.appendTail(sb);
        } else {
            //去除除字母之外的前面带的下划线
            return sb.toString().replaceAll("_", "");
        }
        return underlineToHump(sb.toString());
    }

    /**
     * 驼峰转下划线
     * @param humpStr 字符串
     * @param defaultUppercaseAndTrueLowercase 是否转换
     * @return java.lang.String
     * @author JiaChaoYang
     * @since 2023/2/10 10:49
    */
    public static String humpToLowerLine(String humpStr, boolean ... defaultUppercaseAndTrueLowercase) {
        Matcher matcher = humpPattern.matcher(humpStr);
        StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            matcher.appendReplacement(sb, "_" + matcher.group(0).toLowerCase());
        }
        matcher.appendTail(sb);

        //如果第二个形参为true 转为大写
        if (defaultUppercaseAndTrueLowercase.length>=1 && defaultUppercaseAndTrueLowercase[0]){
            return sb.toString().toUpperCase();
        }
        return sb.toString().substring(4);
    }

}
