package com.anwen.mongo.cache.global;

import com.anwen.mongo.listener.Listener;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 监听器
 * @date 2023-11-22 17:13
 **/
public class ListenerCache {

    public static List<Listener> listeners = new ArrayList<>();

    public static void sorted() {
        listeners = listeners.stream().sorted(Comparator.comparing(Listener::getOrder)).collect(Collectors.toList());
    }

}
