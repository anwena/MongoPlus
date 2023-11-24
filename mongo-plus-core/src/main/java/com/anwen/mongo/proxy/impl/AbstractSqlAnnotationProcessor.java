package com.anwen.mongo.proxy.impl;

import com.anwen.mongo.annotation.mapper.Param;
import com.anwen.mongo.mapper.AbstractMapper;
import com.anwen.mongo.proxy.MapperAnnotationProcessor;
import com.anwen.mongo.toolkit.CollUtil;
import com.anwen.mongo.toolkit.StringUtils;

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

    /**
     * sql模板提供者
     * @return sql模板
     */
    protected abstract Supplier<String> sqlSupplier(Method method);

    @Override
    public Object process(AbstractMapper<?> source, Object proxy, Method method, Object[] args) throws Throwable {
        // 从缓存中读取
        CachedMethodInfo cachedMethodInfo = getCacheByMethod(method, sqlSupplier(method));
        // 替换模板参数
        String realSql = cachedMethodInfo.replaceSqlArgs(args);
        // 执行
        Object result = executeSql(source, proxy, realSql);
        // 获取返回结果
        return cachedMethodInfo.getResultByReturnType(result);
    }

    /**
     * 执行sql
     * @param source 源对象（存在执行sql的功能）
     * @param proxy 代理对象
     * @param realSql 填充过的sql语句
     * @return 执行的返回结果
     */
    protected abstract Object executeSql(AbstractMapper<?> source, Object proxy, String realSql);

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
            cachedMethodInfo.sqlTemplate = sqlSupplier.get();
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
    // private static final Integer RETURN_MAP = 4;

    /**
     * 缓存的方法信息
     */
    static class CachedMethodInfo {
        // 对应方法
        Method method;
        // sql模板
        String sqlTemplate;
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
                    if (!StringUtils.isEmpty(param.value())) {
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
            String sqlTemplate = this.sqlTemplate;
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

        /**
         * 根据返回类型获取结果
         * @param result
         * @return
         */
        public Object getResultByReturnType(Object result) {
            boolean collectable = result instanceof List<?>;

            // 单个的结果
            if (!collectable && returnType.equals(RETURN_ONE)) return result;

            // 多个的结果
            if (collectable) {
                List<?> resultList = (List<?>) result;
                if (returnType.equals(RETURN_ONE)) {
                    if (CollUtil.isEmpty(resultList)) return null;
                    return resultList.get(0);
                } else if (returnType.equals(RETURN_LIST)) {
                    if (CollUtil.isEmpty(resultList)) return new ArrayList<>(0);
                    return resultList;
                } else if (returnType.equals(RETURN_ARRAY)) {
                    Class<?> componentType = method.getReturnType().getComponentType();
                    if (CollUtil.isEmpty(resultList)) return Array.newInstance(componentType, 0);
                    Object[] objects = (Object[]) Array.newInstance(componentType, resultList.size());
                    for (int i = 0; i < objects.length; i++) {
                        objects[i] = resultList.get(i);
                    }
                    return objects;
                }
            }

            // 非法结果
            throw new IllegalArgumentException("The return type is illegal.");
        }
    }
}
