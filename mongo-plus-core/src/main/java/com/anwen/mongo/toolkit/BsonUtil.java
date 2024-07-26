package com.anwen.mongo.toolkit;

import com.anwen.mongo.bson.EmptyDocument;
import com.anwen.mongo.cache.codec.MapCodecCache;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.mongodb.BasicDBObject;
import com.mongodb.DBObject;
import com.mongodb.DBRef;
import com.mongodb.MongoClientSettings;
import org.bson.*;
import org.bson.codecs.configuration.CodecRegistry;
import org.bson.conversions.Bson;
import org.bson.types.Binary;
import org.bson.types.ObjectId;

import java.io.Serializable;
import java.time.*;
import java.time.temporal.Temporal;
import java.util.*;
import java.util.stream.Collectors;

/**
 * BsonUtil
 *
 * @author JiaChaoYang
 **/
public class BsonUtil {

    /**
     * The empty document (immutable). This document is serializable.
     *
     * @since 3.2.5
     */
    public static final Document EMPTY_DOCUMENT = new EmptyDocument();

    @SuppressWarnings("unchecked")
    public static <T> T get(Bson bson, String key) {
        return (T) asMap(bson).get(key);
    }

    /**
     * 将Bson对象作为Map返回。根据输入类型的不同，返回值可以是bson的casted版本，也可以是converted版本（与原始值分离）。
     */
    public static Map<String, Object> asMap(Bson bson) {
        return asMap(bson, MongoClientSettings.getDefaultCodecRegistry());
    }

    /**
     * 将Bson对象作为Map返回。根据输入类型的不同，返回值可以是bson的广播版本，也可以是使用给定CodecRegistry进行转换（与原始值分离）以获得转换所需的编解码器。
     */
    public static Map<String, Object> asMap(Bson bson, CodecRegistry codecRegistry) {

        if (bson == null) {
            return Collections.emptyMap();
        }

        if (bson instanceof Document) {
            return (Document) bson;
        }
        if (bson instanceof BasicDBObject) {
            return (BasicDBObject) bson;
        }
        if (bson instanceof DBObject) {
            return ((DBObject) bson).toMap();
        }

        return new Document((Map) bson.toBsonDocument(Document.class, codecRegistry));
    }

    /**
     * 将Bson对象作为Document返回。根据输入类型的不同，返回值可以是bson的casted版本，也可以是converted版本（与原始值分离）。
     */
    public static Document asDocument(Bson bson) {
        return asDocument(bson, MongoClientSettings.getDefaultCodecRegistry());
    }

    /**
     * 将Bson对象作为Document返回。根据输入类型的不同，返回值可以是bson的广播版本，也可以是使用给定CodecRegistry进行转换（与原始值分离）以获得转换所需的编解码器。
     */
    public static Document asDocument(Bson bson, CodecRegistry codecRegistry) {

        Map<String, Object> map = asMap(bson, codecRegistry);

        if (map instanceof Document) {
            return (Document) bson;
        }

        return new Document(map);
    }

    /**
     * 将Bson对象作为可变文档返回，该文档包含Bson的所有条目。
     */
    public static Document asMutableDocument(Bson bson) {

        if (bson instanceof EmptyDocument) {
            bson = new Document(asDocument(bson));
        }

        if (bson instanceof Document) {
            return (Document) bson;
        }

        Map<String, Object> map = asMap(bson);

        if (map instanceof Document) {
            return (Document) map;
        }

        return new Document(map);
    }

/*    public static void addToMap(Bson bson, String key, Object value) {

        if (bson instanceof Document) {

            ((Document) bson).put(key, value);
            return;
        }
        if (bson instanceof BSONObject) {

            ((BSONObject) bson).put(key, value);
            return;
        }

        throw new IllegalArgumentException(String.format(
                "Cannot add key/value pair to %s; as map given Bson must be a Document or BSONObject", bson.getClass()));
    }*/

    public static Bson addToMap(Bson bson, String key, Object value) {

        if (bson instanceof Document) {

            ((Document) bson).put(key, value);
            return bson;
        }
        if (!(bson instanceof BSONObject)){
            bson = new BasicDBObject(bson.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()));
        }
        ((BSONObject) bson).put(key, value);
        return bson;
    }

    public static Bson addToMapByKey(String targetKey , Bson bson, String key, Object value) {
        BsonDocument bsonDocument = bson.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()).get(targetKey).asDocument();
        if (bsonDocument == null){
            return bson;
        }
        bson = addToMap(bsonDocument, key, value);
        return bson;
    }

    /**
     * 将给定源Map中的所有条目添加到目标
     */
    public static void addAllToMap(Bson target, Map<String, ?> source) {

        if (target instanceof Document) {
            ((Document) target).putAll(source);
            return;
        }
        if (!(target instanceof BSONObject)){
            target = new BasicDBObject(target.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()));
        }
        ((BSONObject) target).putAll(source);
    }

    /**
     * 检查给定Bson中是否存在给定条目（键/ 值对）。
     */
    public static boolean contains(Bson bson, String key, Object value) {

        if (bson instanceof Document) {
            Document document = (Document) bson;
            return document.containsKey(key) && nullSafeEquals(document.get(key), value);
        }
        if (bson instanceof BSONObject) {
            BSONObject bsonObject = (BSONObject) bson;
            return bsonObject.containsField(key) && nullSafeEquals(bsonObject.get(key), value);
        }

        Map<String, Object> map = asMap(bson);
        return map.containsKey(key) && nullSafeEquals(map.get(key), value);
    }

    public static boolean nullSafeEquals(Object o1, Object o2) {
        if (o1 == o2) {
            return true;
        }
        if (o1 == null || o2 == null) {
            return false;
        }
        if (o1.equals(o2)) {
            return true;
        }
        if (o1.getClass().isArray() && o2.getClass().isArray()) {
            return arrayEquals(o1, o2);
        }
        return false;
    }

    private static boolean arrayEquals(Object o1, Object o2) {
        if (o1 instanceof Object[] && o2 instanceof Object[]) {
            return Arrays.equals((Object[]) o1, (Object[]) o2);
        }
        if (o1 instanceof boolean[] && o2 instanceof boolean[]) {
            return Arrays.equals((boolean[]) o1, (boolean[]) o2);
        }
        if (o1 instanceof byte[] && o2 instanceof byte[]) {
            return Arrays.equals((byte[]) o1, (byte[]) o2);
        }
        if (o1 instanceof char[] && o2 instanceof char[]) {
            return Arrays.equals((char[]) o1, (char[]) o2);
        }
        if (o1 instanceof double[] && o2 instanceof double[]) {
            return Arrays.equals((double[]) o1, (double[]) o2);
        }
        if (o1 instanceof float[] && o2 instanceof float[]) {
            return Arrays.equals((float[]) o1, (float[]) o2);
        }
        if (o1 instanceof int[] && o2 instanceof int[]) {
            return Arrays.equals((int[]) o1, (int[]) o2);
        }
        if (o1 instanceof long[] && o2 instanceof long[]) {
            return Arrays.equals((long[]) o1, (long[]) o2);
        }
        if (o1 instanceof short[] && o2 instanceof short[]) {
            return Arrays.equals((short[]) o1, (short[]) o2);
        }
        return false;
    }

    /**
     * 从给定的Bson中移除_id:null（如果存在）。
     */
    public static boolean removeNullId(Bson bson) {

        if (!contains(bson, "_id", null)) {
            return false;
        }

        removeFrom(bson, "_id");
        return true;
    }

    /**
     * 从Bson值中删除给定的key。
     */
    static void removeFrom(Bson bson, String key) {

        if (bson instanceof Document) {

            ((Document) bson).remove(key);
            return;
        }

        if (bson instanceof BSONObject) {

            ((BSONObject) bson).removeField(key);
            return;
        }

        throw new IllegalArgumentException(
                String.format("Cannot remove from %s given Bson must be a Document or BSONObject.", bson.getClass()));
    }

    /**
     * 从BsonValue中提取相应的普通值。例如，BsonString中的纯字符串
     */
    public static Object toJavaType(BsonValue value) {

        switch (value.getBsonType()) {
            case INT32:
                return value.asInt32().getValue();
            case INT64:
                return value.asInt64().getValue();
            case STRING:
                return value.asString().getValue();
            case DECIMAL128:
                return value.asDecimal128().doubleValue();
            case DOUBLE:
                return value.asDouble().getValue();
            case BOOLEAN:
                return value.asBoolean().getValue();
            case OBJECT_ID:
                return value.asObjectId().getValue();
            case DB_POINTER:
                return new DBRef(value.asDBPointer().getNamespace(), value.asDBPointer().getId());
            case BINARY:
                return value.asBinary().getData();
            case DATE_TIME:
                return new Date(value.asDateTime().getValue());
            case SYMBOL:
                return value.asSymbol().getSymbol();
            case ARRAY:
                return value.asArray().toArray();
            case DOCUMENT:
                return Document.parse(value.asDocument().toJson());
            default:
                return value;
        }
    }

    /**
     * 将给定的简单值（例如String、Long）转换为相应的BsonValue。
     */
    public static BsonValue simpleToBsonValue(Object source) {

        if (source instanceof BsonValue) {
            return (BsonValue) source;
        }

        if (source instanceof ObjectId) {
            return new BsonObjectId((ObjectId) source);
        }

        if (source instanceof String) {
            return new BsonString((String) source);
        }

        if (source instanceof Double) {
            return new BsonDouble((Double) source);
        }

        if (source instanceof Integer) {
            return new BsonInt32((Integer) source);
        }

        if (source instanceof Long) {
            return new BsonInt64((Long) source);
        }

        if (source instanceof byte[]) {
            return new BsonBinary((byte[]) source);
        }

        if (source instanceof Boolean) {
            return new BsonBoolean((Boolean) source);
        }

        if (source instanceof Float) {
            return new BsonDouble((Float) source);
        }

        if (source instanceof Binary) {
            Binary binary = (Binary) source;
            return new BsonBinary(binary.getType(), binary.getData());
        }

        if (source instanceof Temporal) {
            if (source instanceof Instant) {
                Instant value = (Instant) source;
                return new BsonDateTime(value.toEpochMilli());
            }
            if (source instanceof LocalDateTime) {
                LocalDateTime value = (LocalDateTime) source;
                return new BsonDateTime(value.toInstant(ZoneOffset.UTC).toEpochMilli());
            }
            if (source instanceof LocalDate) {
                LocalDate value = (LocalDate) source;
                return new BsonDateTime(value.atStartOfDay(ZoneOffset.UTC).toInstant().toEpochMilli());
            }
            if (source instanceof LocalTime) {
                LocalTime value = (LocalTime) source;
                return new BsonDateTime(value.atDate(LocalDate.ofEpochDay(0L)).toInstant(ZoneOffset.UTC).toEpochMilli());
            }
        }

        if (source instanceof Date) {
            Date date = (Date) source;
            new BsonDateTime(date.getTime());
        }

        throw new IllegalArgumentException(String.format("Unable to convert %s (%s) to BsonValue.", source,
                source != null ? source.getClass().getName() : "null"));
    }

    /**
     * 按照给定的顺序将给定的文档合并到上。包含在多个文档中的密钥会被后续文档覆盖。
     */
    public static Document merge(Document... documents) {

        if (ArrayUtils.isEmpty(documents)) {
            return new Document();
        }

        if (documents.length == 1) {
            return documents[0];
        }

        Document target = new Document();
        Arrays.asList(documents).forEach(target::putAll);
        return target;
    }

    /**
     * 检查给定的字符串是否看起来像可解析的json。
     */
    public static boolean isJsonDocument(String value) {

        if (!StringUtils.hasText(value)) {
            return false;
        }

        String potentialJson = value.trim();
        return potentialJson.startsWith("{") && potentialJson.endsWith("}");
    }

    /**
     * 检查给定的 String 是否看起来像 parsable json 数组。
     */
    public static boolean isJsonArray(String value) {
        return StringUtils.hasText(value) && (value.startsWith("[") && value.endsWith("]"));
    }

    /**
     * 解析给定键的值。如果给定 Bson 值包含键，则立即返回该值。如果不是，并且键包含使用点（.）表示法的路径，它将尝试通过检查各个部分来解析路径。如果其中一个中间值为 null 或无法进一步检查（错误）类型，则返回 null。
     */
    public static Object resolveValue(Bson bson, String key) {
        return resolveValue(asMap(bson), key);
    }

    /**
     * 解析给定键的值。如果给定 Map 值包含键，则立即返回该值。如果不是，并且键包含使用点（.）表示法的路径，它将尝试通过检查各个部分来解析路径。如果其中一个中间值为 null 或无法进一步检查（错误）类型，则返回 null。
     */
    public static Object resolveValue(Map<String, Object> source, String key) {

        if (source.containsKey(key) || !key.contains(".")) {
            return source.get(key);
        }

        String[] parts = key.split("\\.");

        for (int i = 1; i < parts.length; i++) {

            Object result = source.get(parts[i - 1]);

            if (!(result instanceof Bson)) {
                return null;
            }

            source = asMap((Bson) result);
        }

        return source.get(parts[parts.length - 1]);
    }

    /**
     * 返回基础 bson 是否具有给定 key的值（null 或非 null）。
     */
    public static boolean hasValue(Bson bson, String key) {

        Map<String, Object> source = asMap(bson);

        if (source.get(key) != null) {
            return true;
        }

        if (!key.contains(".")) {
            return false;
        }

        String[] parts = key.split("\\.");

        Object result;

        for (int i = 1; i < parts.length; i++) {

            result = source.get(parts[i - 1]);
            source = getAsMap(result);

            if (source == null) {
                return false;
            }
        }

        return source.containsKey(parts[parts.length - 1]);
    }

    /**
     * 以 map 的形式返回给定的源对象，即 Documents 和 maps 原样或 null。
     */
    @SuppressWarnings("unchecked")
    private static Map<String, Object> getAsMap(Object source) {

        if (source instanceof Document) {
            return (Document) source;
        }

        if (source instanceof BasicDBObject) {
            return (BasicDBObject) source;
        }

        if (source instanceof DBObject) {
            DBObject dbObject = (DBObject) source;
            return dbObject.toMap();
        }

        if (source instanceof Map) {
            return (Map<String, Object>) source;
        }

        return null;
    }

    /**
     * 返回给定的源对象， Bson即 Documents，并按原样映射或抛出 IllegalArgumentException。
     */
    @SuppressWarnings("unchecked")
    public static Bson asBson(Object source) {

        if (source instanceof Document) {
            return (Document) source;
        }

        if (source instanceof BasicDBObject) {
            return (BasicDBObject) source;
        }

        if (source instanceof DBObject) {
            DBObject dbObject = (DBObject) source;
            return new Document(dbObject.toMap());
        }

        if (source instanceof Map) {
            return new Document((Map<String, Object>) source);
        }

        throw new IllegalArgumentException(String.format("Cannot convert %s to Bson", source));
    }

    /**
     * 返回给定的源可以使用/ 转换为 Bson。
     */
    public static boolean supportsBson(Object source) {
        return source instanceof DBObject || source instanceof Map;
    }

    /**
     * 将给定对象返回为 Collection。如果源已经是 aCollection，则将按原样返回，Collection将数组转换为 a Collection 或只是为其他所有内容创建一个单个元素集合。
     */
    public static Collection<?> asCollection(Object source) {

        if (source instanceof Collection<?>) {
            return (Collection<?>) source;
        }

        return source.getClass().isArray() ? CollUtil.arrayToList(source) : Collections.singleton(source);
    }

    private static String serializeValue(Object value) {

        if (value == null) {
            return "null";
        }

        String documentJson = new Document("toBeEncoded", value).toJson();
        return documentJson.substring(documentJson.indexOf(':') + 1, documentJson.length() - 1).trim();
    }

    public static Bson getIdsCondition(Collection<? extends Serializable> idList){
        List<Object> convertedIds = idList.stream()
                .map(StringUtils::getObjectIdValue)
                .collect(Collectors.toList());
        return Filters.in(SqlOperationConstant._ID, convertedIds);
    }
}
