package com.anwen.mongo.registrar;

import com.anwen.mongo.annotation.MapperScan;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.mapper.MapperScanner;
import com.anwen.mongo.proxy.MapperInvokeHandler;
import com.anwen.mongo.proxy.MapperProxy;
import com.anwen.mongo.toolkit.StringUtils;
import org.noear.solon.Solon;
import org.noear.solon.core.AppContext;
import org.noear.solon.core.BeanWrap;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * @Description: mapper注册器
 * @Name: MongoPlusMapperRegistrar
 * @Author: Bomber
 * @CreateTime: 2023/11/21 16:39
 */
public class MongoPlusMapperRegistrar extends MapperScanner {

    private final SqlExecute sqlExecute;

    public MongoPlusMapperRegistrar(SqlExecute sqlExecute) {
        this.sqlExecute = sqlExecute;
        try {
            // 获取启动类
            String startAppClassName = Solon.cfg().get("sun.java.command");
            Class<?> startAppClass = Class.forName(startAppClassName);

            // 创建包扫描路径
            String defaultPackage = startAppClass.getPackage().getName();
            List<String> packages = new LinkedList<>();
            packages.add(defaultPackage);
            if (startAppClass.isAnnotationPresent(MapperScan.class)) {
                MapperScan annotation = startAppClass.getAnnotation(MapperScan.class);
                String[] basePackages = annotation.basePackages();
                for (int i = 0; i < basePackages.length; i++) {
                    if (StringUtils.isNotEmpty(basePackages[i])) packages.add(basePackages[i]);
                }
            }

            // 扫描
            Set<Class<?>> mapperClasses = super.scanMappers(packages);

            // 注册
            this.registerMappers(mapperClasses);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    /**
     * 注册mapper
     * @param mapperClasses
     */
    @SuppressWarnings("unchecked")
    private void registerMappers(Set<Class<?>> mapperClasses) {
        AppContext context = Solon.context();
        mapperClasses.forEach(mapperClass -> {
            try {
                MapperProxy mapperProxy = new MapperProxy<>();
                mapperProxy.setMapperClass(mapperClass);
                BaseMapper baseMapperInstance = mapperProxy.getObject();
                MapperInvokeHandler<?> invocationHandler = (MapperInvokeHandler<?>) Proxy.getInvocationHandler(baseMapperInstance);
                invocationHandler.setSqlExecute(sqlExecute);
                BeanWrap beanWrap = new BeanWrap(context, mapperClass, baseMapperInstance);

                String beanName = mapperClass.getSimpleName().substring(0, 1).toLowerCase() + mapperClass.getSimpleName().substring(1);
                context.beanRegister(beanWrap, beanName, true);
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }
}
