package com.anwen.mongo.aggregate.pipeline;

import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.support.SFunction;

/**
 * $project阶段
 * @author anwen
 * @date 2024/6/10 下午4:45
 */
public interface Project<Children> {

    /**
     * 显示哪些字段
     * @param column 列名，字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
     */
    <T,R> Children projectDisplay(SFunction<T,R>... column);

    /**
     * 显示哪些字段
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
     */
    Children projectDisplay(String... column);

    /**
     * 不显示哪些字段
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
     */
    <T,R> Children projectNone(SFunction<T,R>... column);

    /**
     * 不显示哪些字段
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:38
     */
    Children projectNone(String... column);

    /**
     * 要显示哪写字段或者不显示哪些字段
     * @param displayId 是否显示_id
     * @param projection 对象
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:36
     */
    Children project(boolean displayId, Projection... projection);

    /**
     * 显示哪些字段
     * @param displayId 是否显示_id
     * @param column 列名，字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
     */
    <T,R> Children projectDisplay(boolean displayId,SFunction<T,R>... column);

    /**
     * 显示哪些字段
     * @param displayId 是否显示_id
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
     */
    Children projectDisplay(boolean displayId,String... column);

    /**
     * 不显示哪些字段
     * @param displayId 是否显示_id
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
     */
    <T,R> Children projectNone(boolean displayId,SFunction<T,R>... column);

    /**
     * 不显示哪些字段
     * @param displayId 是否显示_id
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:38
     */
    Children projectNone(boolean displayId,String... column);

}
