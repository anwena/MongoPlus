package com.anwen.mongo.interceptor;

import com.anwen.mongo.model.command.CommandFailed;
import com.anwen.mongo.model.command.CommandStarted;
import com.anwen.mongo.model.command.CommandSucceeded;
import com.mongodb.event.CommandFailedEvent;
import com.mongodb.event.CommandListener;
import com.mongodb.event.CommandStartedEvent;
import com.mongodb.event.CommandSucceededEvent;

public class BaseInterceptor implements CommandListener {

    private final MongoPlusInterceptor mongoPlusInterceptor = new MongoPlusInterceptor();

    @Override
    public void commandStarted(CommandStartedEvent event) {
        mongoPlusInterceptor.commandStarted(new CommandStarted(event.getCommandName(),event.getCommand(),event.getCommand().toJson(),event));
    }

    @Override
    public void commandSucceeded(CommandSucceededEvent event) {
        mongoPlusInterceptor.commandSucceeded(new CommandSucceeded(event.getCommandName(),event.getResponse(),event));
    }

    @Override
    public void commandFailed(CommandFailedEvent event) {
        mongoPlusInterceptor.commandFailed(new CommandFailed(event.getCommandName(),event.getThrowable(),event));
    }
}
