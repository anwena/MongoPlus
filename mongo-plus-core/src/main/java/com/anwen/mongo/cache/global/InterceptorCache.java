package com.anwen.mongo.cache.global;

import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.interceptor.business.TenantInterceptor;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 拦截器
 *
 * @author JiaChaoYang
 **/
public class InterceptorCache {

    public static List<Interceptor> interceptors = new ArrayList<>();

    public static void sorted() {
        interceptors = interceptors.stream().sorted(Comparator.comparing(Interceptor::order)).collect(Collectors.toList());
    }

    public static Interceptor getTenant(){
        return interceptors.stream().filter(interceptor -> interceptor instanceof TenantInterceptor).findFirst().orElse(null);
    }

}
