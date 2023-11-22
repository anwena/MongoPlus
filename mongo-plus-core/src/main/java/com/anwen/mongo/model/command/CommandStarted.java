package com.anwen.mongo.model.command;

import com.mongodb.event.CommandStartedEvent;
import org.bson.BsonDocument;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 命令开始信息
 * @date 2023-11-22 14:17
 **/
public class CommandStarted extends BaseCommand {

    /**
     * 命令BsonDocument类型
     * @author JiaChaoYang
     * @date 2023/11/22 14:26
    */
    private BsonDocument commandDocument;

    /**
     * 命令，解析为json的类型
     * @author JiaChaoYang
     * @date 2023/11/22 14:26
    */
    private String command;

    /**
     * MongoDB提供，比较全的开始命令
     * @author JiaChaoYang
     * @date 2023/11/22 14:27
    */
    private CommandStartedEvent commandStartedEvent;

    public BsonDocument getCommandDocument() {
        return commandDocument;
    }

    public void setCommandDocument(BsonDocument commandDocument) {
        this.commandDocument = commandDocument;
    }

    public String getCommand() {
        return command;
    }

    public void setCommand(String command) {
        this.command = command;
    }

    public CommandStartedEvent getCommandStartedEvent() {
        return commandStartedEvent;
    }

    public void setCommandStartedEvent(CommandStartedEvent commandStartedEvent) {
        this.commandStartedEvent = commandStartedEvent;
    }

    public CommandStarted(String commandName, BsonDocument commandDocument, String command, CommandStartedEvent commandStartedEvent) {
        super(commandName);
        this.commandDocument = commandDocument;
        this.command = command;
        this.commandStartedEvent = commandStartedEvent;
    }

    public CommandStarted() {
    }
}
