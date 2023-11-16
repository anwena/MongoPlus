package com.anwen.mongo.registrar;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.BeanNameGenerator;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.type.AnnotationMetadata;

/**
 * @Description: 额外的Bean注册器
 * @Name: MongoPlusRegistrar
 * @Author: Bomber
 * @CreateTime: 2023/11/16 15:51
 */
public class MongoPlusRegistrar implements ImportBeanDefinitionRegistrar {

    Logger logger = LoggerFactory.getLogger(MongoPlusRegistrar.class);

    @Override
    public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry, BeanNameGenerator importBeanNameGenerator) {
        logger.info("scan mappers");
        // 获取启动类的包路径
        importingClassMetadata.getClassName();
    }
}
