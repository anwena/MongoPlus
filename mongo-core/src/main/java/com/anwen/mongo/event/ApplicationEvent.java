package com.anwen.mongo.event;

import java.io.Serializable;

/**
 * 抽象事件结构（空结构），只为了约束定义事件
 * @author JiaChaoYang
 * @date 2023/8/28 20:06
*/
public abstract class ApplicationEvent implements Serializable {

    private static final long serialVersionUID = -1L;


}
