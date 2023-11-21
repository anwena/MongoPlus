package com.anwen.mongo.mapper;

import org.reflections.Reflections;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @Description: mapper扫描器
 * @Name: MapperScanner
 * @Author: Bomber
 * @CreateTime: 2023/11/21 15:18
 */
public class MapperScanner {

    /**
     * 扫描mapper接口
     * @param packages 包集合
     * @return
     */
    protected Set<Class<?>> scanMappers(List<String> packages) {
        Set<Class<?>> classes = new HashSet<>();
        packages.forEach(p -> {
            Reflections reflections = new Reflections(p);
            classes.addAll(reflections.getSubTypesOf(BaseMapper.class));
        });
        return classes;
    }
}
