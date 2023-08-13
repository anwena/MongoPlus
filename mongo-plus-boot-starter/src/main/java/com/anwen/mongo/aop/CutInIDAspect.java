package com.anwen.mongo.aop;

import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.toolkit.Generate;
import com.anwen.mongo.toolkit.StringUtils;
import lombok.extern.log4j.Log4j2;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;

import static com.anwen.mongo.toolkit.AnnotationUtil.getFieldAnnotation;

/**
 * @author JiaChaoYang
 * id切入
 * @since 2023-02-13 14:27
 **/
@Aspect
@Component
@Log4j2
public class CutInIDAspect {

    @Pointcut("@annotation(com.anwen.mongo.annotation.CutInID)")
    public void idAspect(){}


    @Before("idAspect()")
    public void around(JoinPoint joinPoint){
        //修改传入参数
        processInputArg(joinPoint.getArgs());
    }

    /**
     * 处理输入参数
     *
     * @param args 入参列表
     */
    private <T> void processInputArg(Object[] args) {
        for (Object arg : args) {
            T entity = (T)arg;
            if (entity instanceof Collection){
                List<T> entityList = (List<T>) entity;
                entityList.forEach(this::inputArg);
            }else {
                inputArg(entity);
            }
        }
    }

    private <T> void inputArg(T entity){
        Class<?> entityClass = entity.getClass();
        Map<String, Object> fieldAnnotation = getFieldAnnotation(entity);
        if (!fieldAnnotation.containsKey("fieldName") && !fieldAnnotation.containsKey("fieldType") && !fieldAnnotation.containsKey("generateType")){
            return;
        }
        IdTypeEnum idTypeEnum = (IdTypeEnum) fieldAnnotation.get("generateType");
        Class<?> typeClass = (Class<?>) fieldAnnotation.get("fieldType");
        String fieldName = parseSetMethodName(String.valueOf(fieldAnnotation.get("fieldName")));
        Method fieldSetMethod = null;
        try {
            fieldSetMethod = entityClass.getMethod(fieldName, typeClass);
            fieldSetMethod.invoke(entity, getClassTypeValue(typeClass, Generate.generateId(idTypeEnum)));
        } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 拼接在某属性的 set方法
     * @param fieldName 字段名
     * @return java.lang.String
     * @author JiaChaoYang
     * @since 2023/2/13 14:52
    */
    public static String parseSetMethodName(String fieldName) {
        if (StringUtils.isBlank(fieldName)) {
            return null;
        }
        StringBuilder methodName = new StringBuilder();
        methodName.append("set");
        if(fieldName.charAt(0) >= 97 && fieldName.charAt(0) <= 122 &&
                fieldName.charAt(1) >= 65 && fieldName.charAt(1) <= 90) {
            methodName.append(fieldName.charAt(0));
        } else {
            methodName.append(fieldName.substring(0, 1).toUpperCase());
        }
        methodName.append(fieldName.substring(1));
        return methodName.toString();
    }

    /**
     * 通过class类型获取获取对应类型的值
     *
     * @param typeClass class类型
     * @param value     值
     * @return Object
     */
    private static Object getClassTypeValue(Class<?> typeClass, String value) {
        String fieldType = typeClass.getSimpleName();
        switch (fieldType) {
            case "String":
                return value;
            case "Date":
                return parseDate(value);
            case "Integer":
            case "int":
                return Integer.parseInt(value);
            case "Long":
            case "long":
                return Long.parseLong(value);
            case "Double":
            case "double":
                return Double.parseDouble(value);
            case "Float":
            case "float":
                return Float.parseFloat(value);
            case "Boolean":
            case "boolean":
                return Boolean.parseBoolean(value);
            default:
                log.error("NO SUPPER TYPE!!!{}",fieldType);
                return typeClass.cast(value);
        }
    }

    /**
     * 格式化string为Date
     *
     * @param dateStr
     * @return date
     */
    private static Date parseDate(String dateStr) {
        if (dateStr == null || "".equals(dateStr)) {
            return null;
        }
        try {
            String fmtStr;
            if (dateStr.contains(":")) {
                fmtStr = "yyyy-MM-dd HH:mm:ss";
            } else {
                fmtStr = "yyyy-MM-dd";
            }
            SimpleDateFormat sdf = new SimpleDateFormat(fmtStr);
            return sdf.parse(dateStr);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }


}
