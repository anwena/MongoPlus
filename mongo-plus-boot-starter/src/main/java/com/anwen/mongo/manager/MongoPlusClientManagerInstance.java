package com.anwen.mongo.manager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2024-01-05 15:31
 **/
public class MongoPlusClientManagerInstance extends MongoPlusClientManager implements DisposableBean {

    Logger logger = LoggerFactory.getLogger(MongoPlusClientManagerInstance.class);

    @Override
    public void destroy() throws Exception {
        mongoPlusClientMap.forEach((k,v) -> {
            if (logger.isDebugEnabled()){
                logger.debug("Destroy data source connection client: {}",k);
            }
            v.getMongoClient().close();
        });
    }
}
