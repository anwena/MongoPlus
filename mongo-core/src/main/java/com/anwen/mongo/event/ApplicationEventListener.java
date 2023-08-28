package com.anwen.mongo.event;

/**
 * 事件监听接口
 * @author JiaChaoYang
 * @date 2023/8/28 20:06
*/
public interface ApplicationEventListener<ApplicationEvent> {

    /**
     * 收到事件后处理
     * @author JiaChaoYang
     * @date 2023/8/28 20:06
    */
    void onApplicationEvent(ApplicationEvent event);

}
