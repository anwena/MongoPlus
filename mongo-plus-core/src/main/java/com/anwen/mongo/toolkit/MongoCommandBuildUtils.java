package com.anwen.mongo.toolkit;

import com.anwen.mongo.enums.CommandEnum;
import com.mongodb.Function;
import com.mongodb.event.CommandStartedEvent;
import org.bson.BsonArray;
import org.bson.BsonDocument;
import org.bson.BsonValue;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 根据 命令 构建 mongo 语句
 *
 * @author loser
 * @date 2024/5/8
 */
public class MongoCommandBuildUtils {

    private static final Map<String, Function<CommandStartedEvent, String>> HANDLERS = new HashMap<>();

    static {
        HANDLERS.put(CommandEnum.FIND.getCommand(), MongoCommandBuildUtils::buildFindCommand);
        HANDLERS.put(CommandEnum.COUNT.getCommand(), MongoCommandBuildUtils::buildCountCommand);
        HANDLERS.put(CommandEnum.INSERT.getCommand(), MongoCommandBuildUtils::buildInsertCommand);
        HANDLERS.put(CommandEnum.DELETE.getCommand(), MongoCommandBuildUtils::buildDeleteCommand);
        HANDLERS.put(CommandEnum.UPDATE.getCommand(), MongoCommandBuildUtils::buildUpdateCommand);
        HANDLERS.put(CommandEnum.AGGREGATE.getCommand(), MongoCommandBuildUtils::buildAggregateCommand);
    }

    /**
     * 构建语句
     *
     * @param event mongo 命令
     * @return 可执行的 json 命令
     */
    public static String buildCommand(CommandStartedEvent event) {

        Function<CommandStartedEvent, String> function = HANDLERS.get(event.getCommandName());
        if (Objects.nonNull(function)) {
            return function.apply(event);
        }
        return event.getCommand().toString();

    }

    private static String buildFindCommand(CommandStartedEvent event) {

        String collection = getJson("find", "", event);
        String json = getJson("filter", "", event);
        String skip = getJson("skip", "", event);
        String limit = getJson("limit", "", event);
        String command = "db." + collection + "." + event.getCommandName() + "(" + json + ")";
        if (StringUtils.isNotBlank(skip) && !skip.equals("0")) {
            command += ".skip(" + skip + ")";
        }
        if (StringUtils.isNotBlank(limit) && !limit.equals("0")) {
            command += ".limit(" + limit + ")";
        }
        return command;

    }

    private static String buildInsertCommand(CommandStartedEvent event) {

        String json = getJson("documents", "", event);
        String collection = getJson("insert", "", event);
        return "db." + collection + ".insert" + "(" + json + ")";

    }

    private static String buildUpdateCommand(CommandStartedEvent event) {

        String collection = getJson("update", "", event);
        String multi;
        BsonValue bsonValue = event.getCommand().get("updates");
        if (bsonValue instanceof BsonArray) {
            BsonArray array = (BsonArray) bsonValue;
            int size = array.size();
            // 构建批量
            if (size > 1) {
                List<String> res = new ArrayList<>();
                for (BsonValue value : array) {
                    String q = getJson(value, "q");
                    String u = getJson(value, "u");
                    multi = getJson(value, "multi");
                    if ("true".equals(multi)) {
                        res.add("db." + collection + ".updateMany" + "(" + q + ", " + u + ")");
                    } else {
                        res.add("db." + collection + ".updateOne" + "(" + q + ", " + u + ")");
                    }
                }
                return res.stream().collect(Collectors.joining(System.lineSeparator()));
            }
        }
        multi = getJson("updates", "multi", event);
        String q = getJson("updates", "q", event);
        String u = getJson("updates", "u", event);
        if ("true".equals(multi)) {
            return "db." + collection + ".updateMany" + "(" + q + ", " + u + ")";
        } else {
            return "db." + collection + ".updateOne" + "(" + q + ", " + u + ")";
        }

    }

    private static String buildDeleteCommand(CommandStartedEvent event) {

        String json = getJson("deletes", "q", event);
        String collection = getJson("delete", "", event);
        return "db." + collection + ".deleteMany" + "(" + json + ")";

    }

    private static String buildCountCommand(CommandStartedEvent event) {

        String json = getJson("counts", "q", event);
        String collection = getJson("count", "", event);
        return "db." + collection + ".count" + "(" + json + ")";

    }

    private static String getJson(BsonValue bs, String item) {

        if (Objects.isNull(bs)) {
            return "";
        }
        if (bs.isDocument()) {
            return buildByDocument((BsonDocument) bs, item);
        }
        if (bs.isArray()) {
            return buildByArray((BsonArray) bs, item);
        }
        return BsonUtil.toJavaType(bs).toString();

    }

    private static String buildByArray(BsonArray bs, String item) {

        List<String> jsons = new ArrayList<>();
        for (BsonValue delete : bs) {
            BsonDocument query = delete.asDocument();
            if (StringUtils.isBlank(item)) {
                jsons.add(query.toJson());
                continue;
            }
            BsonValue bsonValue = query.get(item);
            if (Objects.nonNull(bsonValue)) {
                if (bsonValue.isDocument()) {
                    jsons.add(bsonValue.toString());
                } else {
                    jsons.add(BsonUtil.toJavaType(bsonValue).toString());
                }
            }
        }
        if (jsons.isEmpty()) {
            return "";
        } else if (jsons.size() == 1) {
            return jsons.get(0);
        }
        return "[" + String.join(",", jsons) + "]";

    }

    private static String buildByDocument(BsonDocument bs, String item) {

        if (StringUtils.isBlank(item)) {
            return bs.toJson();
        } else {
            BsonValue bsonValue = bs.get(item);
            if (bsonValue instanceof BsonDocument) {
                return bsonValue.asDocument().toJson();
            }
            return BsonUtil.toJavaType(bsonValue).toString();
        }

    }

    private static String getJson(String key, String item, CommandStartedEvent event) {

        BsonDocument filter = event.getCommand();
        BsonValue bs = filter.get(key);
        return getJson(bs, item);

    }

    @SuppressWarnings("unchecked")
    private static String buildAggregateCommand(CommandStartedEvent event) {

        List<BsonDocument> pipeline = (List<BsonDocument>) event.getCommand().get("pipeline");
        String collection = getJson("aggregate", "", event);
        if (CollUtil.isEmpty(pipeline)) {
            return "db." + collection + "." + event.getCommandName() + "()";
        }
        StringBuilder commandBuilder = new StringBuilder("db." + collection + "." + event.getCommandName() + "([");
        for (BsonDocument stage : pipeline) {
            commandBuilder.append(stage.toJson()).append(", ");
        }
        commandBuilder.delete(commandBuilder.length() - 2, commandBuilder.length()).append("])");
        return commandBuilder.toString();

    }

}