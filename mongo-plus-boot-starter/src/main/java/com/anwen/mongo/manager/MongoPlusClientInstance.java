package com.anwen.mongo.manager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2024-01-05 15:31
 **/
public class MongoPlusClientInstance extends MongoPlusClient implements DisposableBean {

    Logger logger = LoggerFactory.getLogger(MongoPlusClientInstance.class);

    @Override
    public void destroy() throws Exception {
        if (logger.isDebugEnabled()){
            logger.debug("Destroy data source connection client");
        }
        getMongoClient().close();
    }
}
