package com.anwen.mongo.event;

import com.anwen.mongo.service.impl.ServiceImpl;
import lombok.extern.log4j.Log4j2;
import org.reflections.Reflections;

import java.io.File;
import java.net.URL;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 单例模式实现事件发布
 * @author JiaChaoYang
 * @date 2023/8/28 20:07
*/
@Log4j2
public class ApplicationEventPublisher {
    private static final Map<String, ApplicationEventListener> map = new ConcurrentHashMap<>();

    /**
     * 利用单例只有一个实例特点，在实例化时执行一次找ApplicationEventListener的动作
     * @author JiaChaoYang
     * @date 2023/8/28 20:07
    */
    private ApplicationEventPublisher() {
        super();
        try {
            Set<Class<? extends ServiceImpl>> classes = loadClassByLoader(this.getClass().getClassLoader());
            classes.forEach(it -> {
                try {
                    map.put(it.getName(), it.newInstance());
                } catch (Exception e) {
                    log.error("error -> ", e);
                }
            });
        } catch (Exception e) {
            log.error("ApplicationEventPublisher error --> ", e);
        }
    }

    /**
     * 单例实现
     * @author JiaChaoYang
     * @date 2023/8/28 20:08
    */
    private static volatile ApplicationEventPublisher instance = null;

    public static synchronized ApplicationEventPublisher getInstance() {
        if (null == instance) {
            synchronized (ApplicationEventPublisher.class) {
                instance = new ApplicationEventPublisher();
            }
        }
        return instance;
    }

    /**
     * 发布事件
     * @param event
     */
    public void publishEvent(ApplicationEvent event) {
        for (ApplicationEventListener value : map.values()) {
            value.onApplicationEvent(event);
        }
        /*map.values().forEach(it -> {
            // 这里可以通过判断是否有注解使用线程池
            CompletableFuture.runAsync(() -> {

            });
        });*/
    }


    /***********    以下代码百度的，可以用    ***********/
    //通过loader加载所有类
    private Set<Class<? extends ServiceImpl>> loadClassByLoader(ClassLoader load) throws Exception{
        Reflections reflections = new Reflections("");
        return reflections.getSubTypesOf(ServiceImpl.class);
    }















    //通过文件路径加载所有类 root 主要用来替换path中前缀（除包路径以外的路径）
    private void loadClassByPath(String root, String path, List<Class> list, ClassLoader load) {
        File f = new File(path);
        if(root==null) root = f.getPath();
        //判断是否是class文件
        if (f.isFile() && f.getName().matches("^.*\\.class$")) {
            try {
                String classPath = f.getPath();
                //截取出className 将路径分割符替换为.（windows是\ linux、mac是/）
                String className = classPath.substring(root.length()+1,classPath.length()-6).replace('/','.').replace('\\','.');
                list.add(load.loadClass(className));
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        } else {
            File[] fs = f.listFiles();
            if (fs == null) return;
            for (File file : fs) {
                loadClassByPath(root,file.getPath(), list, load);
            }
        }
    }
}
