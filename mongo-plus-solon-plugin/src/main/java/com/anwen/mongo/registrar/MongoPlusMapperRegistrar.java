package com.anwen.mongo.registrar;

import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.mapper.MapperScanner;
import com.anwen.mongo.proxy.MapperInvokeHandler;
import com.anwen.mongo.proxy.MapperProxy;
import org.noear.solon.Solon;
import org.noear.solon.core.AppContext;
import org.noear.solon.core.BeanWrap;

import java.lang.reflect.Proxy;
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

        // 创建包扫描路径
        List<String> packages = getBasePackages();
        // 扫描
        Set<Class<?>> mapperClasses = super.scanMappers(packages);
        // 注册
        this.registerMappers(mapperClasses);
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
