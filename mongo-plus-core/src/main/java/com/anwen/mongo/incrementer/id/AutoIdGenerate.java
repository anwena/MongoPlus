package com.anwen.mongo.incrementer.id;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 自增id生成
 * @date 2023-11-24 09:19
 **/
public class AutoIdGenerate {

    private final AtomicInteger counter;

    private volatile static AutoIdGenerate instance;

    /**
     * 双重检查锁定，保证线程安全，基于懒汉
     * @author JiaChaoYang
     * @date 2023/11/24 10:36
    */
    public static AutoIdGenerate getInstance() {
        if (instance == null) {
            synchronized (AutoIdGenerate.class) {
                if (instance == null) {
                    instance = new AutoIdGenerate();
                }
            }
        }
        return instance;
    }

    /**
     * 默认从0开始
     * @author JiaChaoYang
     * @date 2023/11/24 9:22
    */
    private AutoIdGenerate(){
        System.out.println("创建了对象");
        counter = new AtomicInteger(0);
    }

    /**
     * 默认从X开始
     * @author JiaChaoYang
     * @date 2023/11/24 9:22
    */
/*    public AutoIdGenerate(int initialValue){
        counter = new AtomicInteger(initialValue);
    }*/

    /**
     * 获取下一个id
     * @author JiaChaoYang
     * @date 2023/11/24 9:21
    */
    public synchronized int getNextId() {
        int i = counter.incrementAndGet();
        System.out.println("获取id："+i);
        return i;
    }

    public synchronized void addAndGet(int delta){
        counter.addAndGet(delta);
    }

    public static void main(String[] args) {
        AutoIdGenerate autoIdGenerate = new AutoIdGenerate();
        ExecutorService executorService = Executors.newFixedThreadPool(10);
        for (int i=0;i<100;i++){
            executorService.execute(() -> {
                System.out.println(autoIdGenerate.getNextId());
            });
        }
        executorService.shutdown();
    }

}
