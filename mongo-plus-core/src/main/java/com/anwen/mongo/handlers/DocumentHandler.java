package com.anwen.mongo.handlers;

import org.bson.Document;

import java.util.List;

/**
 * Document处理器，添加和修改会经过这里，可以对添加修改的数据进行进一步修改
 * <p style="color: red">注：lambdaUpdate().update()并不会经过这里</p>
 * @author JiaChaoYang
 * @date 2023/11/23 12:50
*/
public interface DocumentHandler {

    /**
     * 处理Document
     * @param document 添加时最终生成的Document
     * @author JiaChaoYang
     * @date 2023/11/23 12:59
    */
    List<Document> insertInvoke(List<Document> document);

    /**
     * 处理Document
     * @param document 添加时最终生成的Document
     * @author JiaChaoYang
     * @date 2023/11/23 12:59
     */
    List<Document> updateInvoke(List<Document> document);

}
