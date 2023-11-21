package com.anwen.mongo.registrar;

import com.anwen.mongo.adaptor.MapperProxyAdaptor;
import com.anwen.mongo.annotation.MapperScan;
import com.anwen.mongo.mapper.MapperScanner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.type.AnnotationMetadata;

import java.util.*;


/**
 * @Description: mapper注册器
 * @Name: MongoPlusMapperRegistrar
 * @Author: Bomber
 * @CreateTime: 2023/11/16 15:51
 */
public class MongoPlusMapperRegistrar extends MapperScanner implements ImportBeanDefinitionRegistrar {

    private final Logger logger = LoggerFactory.getLogger(MongoPlusMapperRegistrar.class);

    @Override
    public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {
        logger.info("scan mappers");
        // 获取启动类包路径
        String appClass = importingClassMetadata.getClassName();
        String appPackage = appClass.substring(0, appClass.lastIndexOf("."));
        // 获取注解标识的包路径
        Map<String, Object> attributes = importingClassMetadata.getAnnotationAttributes(MapperScan.class.getTypeName());
        assert attributes != null;
        String[] basePackages = (String[]) attributes.get("basePackages");

        // 获取mapper类的包路径
        List<String> packages = new LinkedList<>();
        packages.add(appPackage);
        packages.addAll(Arrays.asList(basePackages));

        // 扫描mapper接口
        Set<Class<?>> mapperClasses = scanMappers(packages);
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
