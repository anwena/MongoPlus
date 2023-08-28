package com.anwen.mongo.event;

import com.anwen.mongo.service.impl.ServiceImpl;
import lombok.extern.log4j.Log4j2;

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
            List<Class> classes = loadClassByLoader(this.getClass().getClassLoader());
            classes.forEach(it -> {
                try {
                    if (Arrays.stream(it.getInterfaces()).anyMatch(i -> i == ApplicationEventListener.class || i == ServiceImpl.class)) {
                        log.info("{} is implements ApplicationEventListener", it);
                        map.put(it.getName(), (ApplicationEventListener) it.newInstance());
                    }
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
        map.values().forEach(it -> {
            // 这里可以通过判断是否有注解使用线程池
            CompletableFuture.runAsync(() -> {
                it.onApplicationEvent(event);
            });
        });
    }


    /***********    以下代码百度的，可以用    ***********/
    //通过loader加载所有类
    private List<Class> loadClassByLoader(ClassLoader load) throws Exception{
        Enumeration<URL> urls = load.getResources("");
        //放所有类型
        List<Class> classes = new ArrayList<Class>();
        while (urls.hasMoreElements()) {
            URL url = urls.nextElement();
            //文件类型（其实是文件夹）
            if (url.getProtocol().equals("file")) {
                loadClassByPath(null, url.getPath(), classes, load);
            }
        }
        return classes;
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
