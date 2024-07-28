package com.anwen.mongo.toolkit;

import java.util.Collection;
import java.util.Map;
import java.util.function.Supplier;

/**
 * 有助于验证参数的断言实用程序类。
 * 有助于在运行时尽早清楚地识别程序员错误。
 * 例如，如果公共方法的约定声明不允许null参数，则可以使用Assert来验证该约定。这样做清楚地表明，当发生冲突时会违反约定，并保护类的不变量。
 * 通常用于验证方法参数而不是配置属性，以检查通常是程序员错误而不是配置错误的情况。与配置初始化代码相反，在这种方法中通常没有回退到默认值的意义。
 * 这个类类似于JUnit的断言库。如果一个参数值被认为无效，则抛出IllegalArgumentException（通常）。例如
 * Assert. notNull（clazz，“类不能为null”）；
 * Assert. isTrue（i>0，“该值必须大于零”）；
 * @author JiaChaoYang
 * @since Spring
 **/
public class Assert {

    /**
     * Assert a boolean expression, throwing an {@code IllegalStateException}
     * if the expression evaluates to {@code false}.
     * <p>Call {@link #isTrue} if you wish to throw an {@code IllegalArgumentException}
     * on an assertion failure.
     * <pre class="code">Assert.state(id == null, "The id property must not already be initialized");</pre>
     * @param expression a boolean expression
     * @param message the exception message to use if the assertion fails
     * @throws IllegalStateException if {@code expression} is {@code false}
     */
    public static void state(boolean expression, String message) {
        if (!expression) {
            throw new IllegalStateException(message);
        }
    }

    /**
     * Assert a boolean expression, throwing an {@code IllegalStateException}
     * if the expression evaluates to {@code false}.
     * <p>Call {@link #isTrue} if you wish to throw an {@code IllegalArgumentException}
     * on an assertion failure.
     * <pre class="code">
     * Assert.state(entity.getId() == null,
     *     () -&gt; "ID for entity " + entity.getName() + " must not already be initialized");
     * </pre>
     * @param expression a boolean expression
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails
     * @throws IllegalStateException if {@code expression} is {@code false}
     * @since 5.0
     */
    public static void state(boolean expression, Supplier<String> messageSupplier) {
        if (!expression) {
            throw new IllegalStateException(nullSafeGet(messageSupplier));
        }
    }

    /**
     * Assert a boolean expression, throwing an {@code IllegalArgumentException}
     * if the expression evaluates to {@code false}.
     * <pre class="code">Assert.isTrue(i &gt; 0, "The value must be greater than zero");</pre>
     * @param expression a boolean expression
     * @param message the exception message to use if the assertion fails
     * @throws IllegalArgumentException if {@code expression} is {@code false}
     */
    public static void isTrue(boolean expression, String message) {
        if (!expression) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * Assert a boolean expression, throwing an {@code IllegalArgumentException}
     * if the expression evaluates to {@code false}.
     * <pre class="code">
     * Assert.isTrue(i &gt; 0, () -&gt; "The value '" + i + "' must be greater than zero");
     * </pre>
     * @param expression a boolean expression
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails
     * @throws IllegalArgumentException if {@code expression} is {@code false}
     * @since 5.0
     */
    public static void isTrue(boolean expression, Supplier<String> messageSupplier) {
        if (!expression) {
            throw new IllegalArgumentException(nullSafeGet(messageSupplier));
        }
    }

    /**
     * Assert that an object is {@code null}.
     * <pre class="code">Assert.isNull(value, "The value must be null");</pre>
     * @param object the object to check
     * @param message the exception message to use if the assertion fails
     * @throws IllegalArgumentException if the object is not {@code null}
     */
    public static void isNull( Object object, String message) {
        if (object != null) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * Assert that an object is {@code null}.
     * <pre class="code">
     * Assert.isNull(value, () -&gt; "The value '" + value + "' must be null");
     * </pre>
     * @param object the object to check
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails
     * @throws IllegalArgumentException if the object is not {@code null}
     * @since 5.0
     */
    public static void isNull( Object object, Supplier<String> messageSupplier) {
        if (object != null) {
            throw new IllegalArgumentException(nullSafeGet(messageSupplier));
        }
    }

    /**
     * Assert that an object is not {@code null}.
     * <pre class="code">Assert.notNull(clazz, "The class must not be null");</pre>
     * @param object the object to check
     * @param message the exception message to use if the assertion fails
     * @throws IllegalArgumentException if the object is {@code null}
     */
    public static void notNull( Object object, String message) {
        if (object == null) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * Assert that an object is not {@code null}.
     * <pre class="code">
     * Assert.notNull(entity.getId(),
     *     () -&gt; "ID for entity " + entity.getName() + " must not be null");
     * </pre>
     * @param object the object to check
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails
     * @throws IllegalArgumentException if the object is {@code null}
     * @since 5.0
     */
    public static void notNull( Object object, Supplier<String> messageSupplier) {
        if (object == null) {
            throw new IllegalArgumentException(nullSafeGet(messageSupplier));
        }
    }

    /**
     * Assert that the given String is not empty; that is,
     * it must not be {@code null} and not the empty String.
     * <pre class="code">Assert.hasLength(name, "Name must not be empty");</pre>
     * @param text the String to check
     * @param message the exception message to use if the assertion fails
     * @throws IllegalArgumentException if the text is empty
     * @see StringUtils#hasLength
     */
    public static void hasLength( String text, String message) {
        if (!StringUtils.hasLength(text)) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * Assert that the given String is not empty; that is,
     * it must not be {@code null} and not the empty String.
     * <pre class="code">
     * Assert.hasLength(account.getName(),
     *     () -&gt; "Name for account '" + account.getId() + "' must not be empty");
     * </pre>
     * @param text the String to check
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails
     * @throws IllegalArgumentException if the text is empty
     * @since 5.0
     * @see StringUtils#hasLength
     */
    public static void hasLength( String text, Supplier<String> messageSupplier) {
        if (!StringUtils.hasLength(text)) {
            throw new IllegalArgumentException(nullSafeGet(messageSupplier));
        }
    }

    /**
     * Assert that the given String contains valid text content; that is, it must not
     * be {@code null} and must contain at least one non-whitespace character.
     * <pre class="code">Assert.hasText(name, "'name' must not be empty");</pre>
     * @param text the String to check
     * @param message the exception message to use if the assertion fails
     * @throws IllegalArgumentException if the text does not contain valid text content
     * @see StringUtils#hasText
     */
    public static void hasText( String text, String message) {
        if (!StringUtils.hasText(text)) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * Assert that the given String contains valid text content; that is, it must not
     * be {@code null} and must contain at least one non-whitespace character.
     * <pre class="code">
     * Assert.hasText(account.getName(),
     *     () -&gt; "Name for account '" + account.getId() + "' must not be empty");
     * </pre>
     * @param text the String to check
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails
     * @throws IllegalArgumentException if the text does not contain valid text content
     * @since 5.0
     * @see StringUtils#hasText
     */
    public static void hasText( String text, Supplier<String> messageSupplier) {
        if (!StringUtils.hasText(text)) {
            throw new IllegalArgumentException(nullSafeGet(messageSupplier));
        }
    }

    /**
     * Assert that the given text does not contain the given substring.
     * <pre class="code">Assert.doesNotContain(name, "rod", "Name must not contain 'rod'");</pre>
     * @param textToSearch the text to search
     * @param substring the substring to find within the text
     * @param message the exception message to use if the assertion fails
     * @throws IllegalArgumentException if the text contains the substring
     */
    public static void doesNotContain( String textToSearch, String substring, String message) {
        if (StringUtils.hasLength(textToSearch) && StringUtils.hasLength(substring) &&
                textToSearch.contains(substring)) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * Assert that the given text does not contain the given substring.
     * <pre class="code">
     * Assert.doesNotContain(name, forbidden, () -&gt; "Name must not contain '" + forbidden + "'");
     * </pre>
     * @param textToSearch the text to search
     * @param substring the substring to find within the text
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails
     * @throws IllegalArgumentException if the text contains the substring
     * @since 5.0
     */
    public static void doesNotContain( String textToSearch, String substring, Supplier<String> messageSupplier) {
        if (StringUtils.hasLength(textToSearch) && StringUtils.hasLength(substring) &&
                textToSearch.contains(substring)) {
            throw new IllegalArgumentException(nullSafeGet(messageSupplier));
        }
    }

    /**
     * Assert that an array contains elements; that is, it must not be
     * {@code null} and must contain at least one element.
     * <pre class="code">
     * Assert.notEmpty(array, () -&gt; "The " + arrayType + " array must contain elements");
     * </pre>
     * @param array the array to check
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails
     * @throws IllegalArgumentException if the object array is {@code null} or contains no elements
     * @since 5.0
     */
    public static void notEmpty( Object[] array, Supplier<String> messageSupplier) {
        if (ArrayUtils.isEmpty(array)) {
            throw new IllegalArgumentException(nullSafeGet(messageSupplier));
        }
    }

    /**
     * Assert that an array contains no {@code null} elements.
     * <p>Note: Does not complain if the array is empty!
     * <pre class="code">Assert.noNullElements(array, "The array must contain non-null elements");</pre>
     * @param array the array to check
     * @param message the exception message to use if the assertion fails
     * @throws IllegalArgumentException if the object array contains a {@code null} element
     */
    public static void noNullElements( Object[] array, String message) {
        if (array != null) {
            for (Object element : array) {
                if (element == null) {
                    throw new IllegalArgumentException(message);
                }
            }
        }
    }

    /**
     * Assert that an array contains no {@code null} elements.
     * <p>Note: Does not complain if the array is empty!
     * <pre class="code">
     * Assert.noNullElements(array, () -&gt; "The " + arrayType + " array must contain non-null elements");
     * </pre>
     * @param array the array to check
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails
     * @throws IllegalArgumentException if the object array contains a {@code null} element
     * @since 5.0
     */
    public static void noNullElements( Object[] array, Supplier<String> messageSupplier) {
        if (array != null) {
            for (Object element : array) {
                if (element == null) {
                    throw new IllegalArgumentException(nullSafeGet(messageSupplier));
                }
            }
        }
    }

    /**
     * Assert that a collection contains elements; that is, it must not be
     * {@code null} and must contain at least one element.
     * <pre class="code">Assert.notEmpty(collection, "Collection must contain elements");</pre>
     * @param collection the collection to check
     * @param message the exception message to use if the assertion fails
     * @throws IllegalArgumentException if the collection is {@code null} or
     * contains no elements
     */
    public static void notEmpty( Collection<?> collection, String message) {
        if (CollUtil.isEmpty(collection)) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * Assert that a collection contains elements; that is, it must not be
     * {@code null} and must contain at least one element.
     * <pre class="code">
     * Assert.notEmpty(collection, () -&gt; "The " + collectionType + " collection must contain elements");
     * </pre>
     * @param collection the collection to check
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails
     * @throws IllegalArgumentException if the collection is {@code null} or
     * contains no elements
     * @since 5.0
     */
    public static void notEmpty( Collection<?> collection, Supplier<String> messageSupplier) {
        if (CollUtil.isEmpty(collection)) {
            throw new IllegalArgumentException(nullSafeGet(messageSupplier));
        }
    }

    /**
     * Assert that a collection contains no {@code null} elements.
     * <p>Note: Does not complain if the collection is empty!
     * <pre class="code">Assert.noNullElements(collection, "Collection must contain non-null elements");</pre>
     * @param collection the collection to check
     * @param message the exception message to use if the assertion fails
     * @throws IllegalArgumentException if the collection contains a {@code null} element
     * @since 5.2
     */
    public static void noNullElements( Collection<?> collection, String message) {
        if (collection != null) {
            for (Object element : collection) {
                if (element == null) {
                    throw new IllegalArgumentException(message);
                }
            }
        }
    }

    /**
     * Assert that a collection contains no {@code null} elements.
     * <p>Note: Does not complain if the collection is empty!
     * <pre class="code">
     * Assert.noNullElements(collection, () -&gt; "Collection " + collectionName + " must contain non-null elements");
     * </pre>
     * @param collection the collection to check
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails
     * @throws IllegalArgumentException if the collection contains a {@code null} element
     * @since 5.2
     */
    public static void noNullElements( Collection<?> collection, Supplier<String> messageSupplier) {
        if (collection != null) {
            for (Object element : collection) {
                if (element == null) {
                    throw new IllegalArgumentException(nullSafeGet(messageSupplier));
                }
            }
        }
    }

    /**
     * Assert that a Map contains entries; that is, it must not be {@code null}
     * and must contain at least one entry.
     * <pre class="code">Assert.notEmpty(map, "Map must contain entries");</pre>
     * @param map the map to check
     * @param message the exception message to use if the assertion fails
     * @throws IllegalArgumentException if the map is {@code null} or contains no entries
     */
    public static void notEmpty(Map<?, ?> map, String message) {
        if (CollUtil.isEmpty(map)) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * Assert that a Map contains entries; that is, it must not be {@code null}
     * and must contain at least one entry.
     * <pre class="code">
     * Assert.notEmpty(map, () -&gt; "The " + mapType + " map must contain entries");
     * </pre>
     * @param map the map to check
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails
     * @throws IllegalArgumentException if the map is {@code null} or contains no entries
     * @since 5.0
     */
    public static void notEmpty(Map<?, ?> map, Supplier<String> messageSupplier) {
        if (CollUtil.isEmpty(map)) {
            throw new IllegalArgumentException(nullSafeGet(messageSupplier));
        }
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * <pre class="code">Assert.instanceOf(Foo.class, foo, "Foo expected");</pre>
     * @param type the type to check against
     * @param obj the object to check
     * @param message a message which will be prepended to provide further context.
     * If it is empty or ends in ":" or ";" or "," or ".", a full exception message
     * will be appended. If it ends in a space, the name of the offending object's
     * type will be appended. In any other case, a ":" with a space and the name
     * of the offending object's type will be appended.
     * @throws IllegalArgumentException if the object is not an instance of type
     */
    public static void isInstanceOf(Class<?> type,  Object obj, String message) {
        notNull(type, "Type to check against must not be null");
        if (!type.isInstance(obj)) {
            instanceCheckFailed(type, obj, message);
        }
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * <pre class="code">
     * Assert.instanceOf(Foo.class, foo, () -&gt; "Processing " + Foo.class.getSimpleName() + ":");
     * </pre>
     * @param type the type to check against
     * @param obj the object to check
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails. See {@link #isInstanceOf(Class, Object, String)} for details.
     * @throws IllegalArgumentException if the object is not an instance of type
     * @since 5.0
     */
    public static void isInstanceOf(Class<?> type,  Object obj, Supplier<String> messageSupplier) {
        notNull(type, "Type to check against must not be null");
        if (!type.isInstance(obj)) {
            instanceCheckFailed(type, obj, nullSafeGet(messageSupplier));
        }
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * <pre class="code">Assert.instanceOf(Foo.class, foo);</pre>
     * @param type the type to check against
     * @param obj the object to check
     * @throws IllegalArgumentException if the object is not an instance of type
     */
    public static void isInstanceOf(Class<?> type,  Object obj) {
        isInstanceOf(type, obj, "");
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * <pre class="code">Assert.isAssignable(Number.class, myClass, "Number expected");</pre>
     * @param superType the supertype to check against
     * @param subType the subtype to check
     * @param message a message which will be prepended to provide further context.
     * If it is empty or ends in ":" or ";" or "," or ".", a full exception message
     * will be appended. If it ends in a space, the name of the offending subtype
     * will be appended. In any other case, a ":" with a space and the name of the
     * offending subtype will be appended.
     * @throws IllegalArgumentException if the classes are not assignable
     */
    public static void isAssignable(Class<?> superType,  Class<?> subType, String message) {
        notNull(superType, "Supertype to check against must not be null");
        if (subType == null || !ClassTypeUtil.isTargetClass(superType,subType)) {
            assignableCheckFailed(superType, subType, message);
        }
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * <pre class="code">
     * Assert.isAssignable(Number.class, myClass, () -&gt; "Processing " + myAttributeName + ":");
     * </pre>
     * @param superType the supertype to check against
     * @param subType the subtype to check
     * @param messageSupplier a supplier for the exception message to use if the
     * assertion fails. See {@link #isAssignable(Class, Class, String)} for details.
     * @throws IllegalArgumentException if the classes are not assignable
     * @since 5.0
     */
    public static void isAssignable(Class<?> superType,  Class<?> subType, Supplier<String> messageSupplier) {
        notNull(superType, "Supertype to check against must not be null");
        if (subType == null || !ClassTypeUtil.isTargetClass(superType,subType)) {
            assignableCheckFailed(superType, subType, nullSafeGet(messageSupplier));
        }
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * <pre class="code">Assert.isAssignable(Number.class, myClass);</pre>
     * @param superType the supertype to check
     * @param subType the subtype to check
     * @throws IllegalArgumentException if the classes are not assignable
     */
    public static void isAssignable(Class<?> superType, Class<?> subType) {
        isAssignable(superType, subType, "");
    }


    private static void instanceCheckFailed(Class<?> type,  Object obj,  String msg) {
        String className = (obj != null ? obj.getClass().getName() : "null");
        String result = "";
        boolean defaultMessage = true;
        if (StringUtils.hasLength(msg)) {
            if (endsWithSeparator(msg)) {
                result = msg + " ";
            }
            else {
                result = messageWithTypeName(msg, className);
                defaultMessage = false;
            }
        }
        if (defaultMessage) {
            result = result + ("Object of class [" + className + "] must be an instance of " + type);
        }
        throw new IllegalArgumentException(result);
    }

    private static void assignableCheckFailed(Class<?> superType, Class<?> subType,String msg) {
        String result = "";
        boolean defaultMessage = true;
        if (StringUtils.hasLength(msg)) {
            if (endsWithSeparator(msg)) {
                result = msg + " ";
            }
            else {
                result = messageWithTypeName(msg, subType);
                defaultMessage = false;
            }
        }
        if (defaultMessage) {
            result = result + (subType + " is not assignable to " + superType);
        }
        throw new IllegalArgumentException(result);
    }

    private static boolean endsWithSeparator(String msg) {
        return (msg.endsWith(":") || msg.endsWith(";") || msg.endsWith(",") || msg.endsWith("."));
    }

    private static String messageWithTypeName(String msg, Object typeName) {
        return msg + (msg.endsWith(" ") ? "" : ": ") + typeName;
    }

    private static String nullSafeGet(Supplier<String> messageSupplier) {
        return (messageSupplier != null ? messageSupplier.get() : null);
    }


}
