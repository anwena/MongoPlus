package com.anwen.mongo.mapper;

import com.anwen.mongo.annotation.mapper.MapperScan;
import com.anwen.mongo.toolkit.StringUtils;
import org.reflections.Reflections;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * @Description: mapper扫描器
 * @Name: MapperScanner
 * @Author: Bomber
 * @CreateTime: 2023/11/21 15:18
 */
public class MapperScanner {

    private static final Logger logger = LoggerFactory.getLogger(MapperScanner.class);

    /**
     * 获取启动类
     * @return 启动类
     */
    protected Class<?> getApplicationClass() {
        try {
            String applicationClassName = System.getProperty("sun.java.command");
            return Class.forName(applicationClassName);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * 获取扫描路径集合
     * @return 扫描路径集合
     */
    protected List<String> getBasePackages() {
        Class<?> applicationClass = getApplicationClass();
        String defaultPackage = applicationClass.getPackage().getName();

        List<String> packages = new LinkedList<>();
        packages.add(defaultPackage);

        if (applicationClass.isAnnotationPresent(MapperScan.class)) {
            MapperScan annotation = applicationClass.getAnnotation(MapperScan.class);
            String[] basePackages = annotation.basePackages();
            for (String basePackage : basePackages) {
                if (StringUtils.isNotEmpty(basePackage)) packages.add(basePackage);
            }
        }

        return packages;
    }

    /**
     * 扫描mapper接口
     * @param packages 包集合
     * @return
     */
    protected Set<Class<?>> scanMappers(List<String> packages) {
        logger.info("scan mappers");
        Set<Class<?>> classes = new HashSet<>();
        packages.forEach(p -> {
            Reflections reflections = new Reflections(p);
            classes.addAll(reflections.getSubTypesOf(BaseMapper.class));
        });
        return classes;
    }
}
