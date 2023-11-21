package com.anwen.mongo.proxy.impl;

import com.anwen.mongo.annotation.mapper.Param;
import com.anwen.mongo.mapper.AbstractMapper;
import com.anwen.mongo.proxy.MapperAnnotationProcessor;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/**
 * @Description: 抽象的Sql注解处理器
 * @Name: AbstractSqlAnnotationProcessor
 * @Author: Bomber
 * @CreateTime: 2023/11/20 15:30
 */
public abstract class AbstractSqlAnnotationProcessor implements MapperAnnotationProcessor {

    // 缓存每个方法上的sql信息
    protected Map<Method, CachedMethodInfo> cachedMethodInfoMap = new ConcurrentHashMap<>();

    @Override
    public Object process(AbstractMapper<?> source, Object proxy, Method method, Object[] args) throws Throwable {
        // 从缓存中读取
        CachedMethodInfo cachedMethodInfo = getCacheByMethod(method, sqlSupplier(method));
        // 替换模板参数
        String realSql = cachedMethodInfo.replaceSqlArgs(args);
        // 执行
        List<?> resultList = source.sql(realSql);
        // 判断结果类型
        if (cachedMethodInfo.returnType.equals(RETURN_ONE)) {
            if (CollectionUtils.isEmpty(resultList)) return null;
            return resultList.get(0);
        } else if (cachedMethodInfo.returnType.equals(RETURN_LIST)) {
            if (CollectionUtils.isEmpty(resultList)) return new ArrayList<>(0);
            return resultList;
        } else if (cachedMethodInfo.returnType.equals(RETURN_ARRAY)) {
            Class<?> componentType = method.getReturnType().getComponentType();
            if (CollectionUtils.isEmpty(resultList)) return Array.newInstance(componentType, 0);
            Object[] objects = (Object[]) Array.newInstance(componentType, resultList.size());
            for (int i = 0; i < objects.length; i++) {
                objects[i] = resultList.get(i);
            }
            return objects;
        }
        throw new IllegalArgumentException("The return type is illegal.");
    }

    /**
     * sql模板提供者
     * @return sql模板
     */
    protected abstract Supplier<String> sqlSupplier(Method method);

    /**
     * 通过方法获取获取sql模板
     * @param method 方法对象
     * @param sqlSupplier sql提供者
     * @return sql模板
     */
    protected CachedMethodInfo getCacheByMethod(Method method, Supplier<String> sqlSupplier) {
        CachedMethodInfo cachedMethodInfo = cachedMethodInfoMap.get(method);
        if (Objects.isNull(cachedMethodInfo)) {
            // 进行缓存
            cachedMethodInfo = new CachedMethodInfo();
            cachedMethodInfo.sqltemplate = sqlSupplier.get();
            cachedMethodInfo.method = method;
            cachedMethodInfo.cacheArgMap();
            cachedMethodInfo.cacheReturnType();
            cachedMethodInfoMap.put(method, cachedMethodInfo);
        }
        return cachedMethodInfo;
    }

    // 放回类型
    private static final Integer RETURN_ONE = 1;
    private static final Integer RETURN_LIST = 2;
    private static final Integer RETURN_ARRAY = 3;

    /**
     * 缓存的方法信息
     */
    static class CachedMethodInfo {
        // 对应方法
        Method method;
        // sql模板
        String sqltemplate;
        // 参数模板下标对照map
        Map<String, Integer> argMap;
        // 返回数量类型
        Integer returnType;

        /**
         * 缓存参数map
         */
        public void cacheArgMap() {
            Map<String, Integer> argIndexMap = new HashMap<>();
            Parameter[] parameters = method.getParameters();
            for (int i = 0; i < parameters.length; i++) {
                // 默认先使用方法的参数名称
                String argName = parameters[i].getName();
                if (parameters[i].isAnnotationPresent(Param.class)) {
                    Param param = parameters[i].getAnnotation(Param.class);
                    if (StringUtils.hasText(param.value())) {
                        argName = param.value();
                    }
                }
                argIndexMap.put(argName, i);
            }
            this.argMap = argIndexMap;
        }

        /**
         * 替换sql模板中的参数
         * @param args 参数
         * @return 真实sql
         */
        public String replaceSqlArgs(Object[] args) {
            // 循环替换
            String sqlTemplate = this.sqltemplate;
            for (Map.Entry<String, Integer> entry : argMap.entrySet()) {
                sqlTemplate = sqlTemplate.replace("#{" + entry.getKey() + "}", args[entry.getValue()].toString());
            }
            return sqlTemplate;
        }

        /**
         * 缓存返回类型
         */
        public void cacheReturnType() {
            Class<?> returnType = method.getReturnType();
            this.returnType = RETURN_ONE;
            if (returnType.getTypeName().endsWith("[]")) {
                this.returnType = RETURN_ARRAY;
                return;
            }
            if (returnType.getTypeName().startsWith(List.class.getTypeName()) ||
                returnType.getTypeName().startsWith(Collection.class.getTypeName()) ||
                returnType.getTypeName().startsWith(Iterator.class.getTypeName()) ||
                returnType.getTypeName().startsWith(ArrayList.class.getTypeName())) {
                this.returnType = RETURN_LIST;
            }
        }
    }
}
