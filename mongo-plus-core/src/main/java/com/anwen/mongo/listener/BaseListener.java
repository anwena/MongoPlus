package com.anwen.mongo.listener;

import com.anwen.mongo.domain.MongoPlusInterceptorException;
import com.anwen.mongo.model.command.CommandFailed;
import com.anwen.mongo.model.command.CommandStarted;
import com.anwen.mongo.model.command.CommandSucceeded;
import com.mongodb.event.CommandFailedEvent;
import com.mongodb.event.CommandListener;
import com.mongodb.event.CommandStartedEvent;
import com.mongodb.event.CommandSucceededEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BaseListener implements CommandListener {

    private final MongoPlusListener mongoPlusInterceptor = new MongoPlusListener();

    private final Logger logger = LoggerFactory.getLogger(BaseListener.class);

    @Override
    public void commandStarted(CommandStartedEvent event) {
        try {
            mongoPlusInterceptor.commandStarted(new CommandStarted(event.getCommandName(),event.getCommand(),event.getCommand().toJson(),event));
        }catch (Exception e){
            logger.error("interceptor error: ",e);
            throw new MongoPlusInterceptorException(e);
        }
    }

    @Override
    public void commandSucceeded(CommandSucceededEvent event) {
        try {
            mongoPlusInterceptor.commandSucceeded(new CommandSucceeded(event.getCommandName(),event.getResponse(),event));
        }catch (Exception e){
            logger.error("interceptor error: ",e);
            throw new MongoPlusInterceptorException(e);
        }
    }

    @Override
    public void commandFailed(CommandFailedEvent event) {
        try {
            mongoPlusInterceptor.commandFailed(new CommandFailed(event.getCommandName(),event.getThrowable(),event));
        }catch (Exception e){
            logger.error("interceptor error: ",e);
            throw new MongoPlusInterceptorException(e);
        }
    }
}
