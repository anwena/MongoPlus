package com.anwen.mongo.model.command;

import com.mongodb.event.CommandFailedEvent;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 命令失败信息
 * @date 2023-11-22 14:30
 **/
public class CommandFailed extends BaseCommand {

    /**
     * 命令失败的异常
     * @author JiaChaoYang
     * @date 2023/11/22 14:31
    */
    private Throwable throwable;

    /**
     * MongoDB提供，比较全的失败信息
     * @author JiaChaoYang
     * @date 2023/11/22 14:32
    */
    private CommandFailedEvent commandFailedEvent;

    public Throwable getThrowable() {
        return throwable;
    }

    public void setThrowable(Throwable throwable) {
        this.throwable = throwable;
    }

    public CommandFailedEvent getCommandFailedEvent() {
        return commandFailedEvent;
    }

    public void setCommandFailedEvent(CommandFailedEvent commandFailedEvent) {
        this.commandFailedEvent = commandFailedEvent;
    }

    public CommandFailed(String commandName, Throwable throwable, CommandFailedEvent commandFailedEvent) {
        super(commandName);
        this.throwable = throwable;
        this.commandFailedEvent = commandFailedEvent;
    }
}
