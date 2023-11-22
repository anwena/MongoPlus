package com.anwen.mongo.registrar;

import com.anwen.mongo.adaptor.MapperProxyAdaptor;
import com.anwen.mongo.mapper.MapperScanner;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.type.AnnotationMetadata;

import java.util.List;
import java.util.Set;


/**
 * @Description: mapper注册器
 * @Name: MongoPlusMapperRegistrar
 * @Author: Bomber
 * @CreateTime: 2023/11/16 15:51
 */
public class MongoPlusMapperRegistrar extends MapperScanner implements ImportBeanDefinitionRegistrar {

    @Override
    public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {
        // 获取包集合
        List<String> packages = getBasePackages();
        // 扫描mapper接口
        Set<Class<?>> mapperClasses = scanMappers(packages);
        // 注册
        registerMappers(mapperClasses, registry);
    }

    /**
     * 注册mapper到spring
     * @param mapperClasses
     * @param registry
     */
    private void registerMappers(Set<Class<?>> mapperClasses, BeanDefinitionRegistry registry) {
        mapperClasses.forEach(mapperClass -> {
            GenericBeanDefinition beanDefinition = new GenericBeanDefinition();
            beanDefinition.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_BY_TYPE);
            beanDefinition.setBeanClass(MapperProxyAdaptor.class);
            beanDefinition.getPropertyValues().add("mapperClass", mapperClass);
            beanDefinition.setPrimary(true);
            beanDefinition.setLazyInit(false);
            beanDefinition.setScope("singleton");

            // 生成bean的名字
            String beanName = mapperClass.getSimpleName().substring(0, 1).toLowerCase() + mapperClass.getSimpleName().substring(1);
            registry.registerBeanDefinition(beanName, beanDefinition);
        });
    }
}
