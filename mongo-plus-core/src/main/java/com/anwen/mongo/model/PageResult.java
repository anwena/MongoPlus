package com.anwen.mongo.model;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * 分页结果
 * 新增分页的多项属性，主要参考: <a href="https://github.com/pagehelper/Mybatis-PageHelper/blob/master/src/main/java/com/github/pagehelper/PageInfo.java">PageHelper-PageInfo</a>
 * @author anwen
 * @date 2024/7/26 下午5:28
 */
public class PageResult<T> implements Serializable {

    private static final long serialVersionUID = 4737078483619467352L;

    /**
     * 当前页
     * @date 2023/2/23 10:58
     */
    private long pageNum;

    /**
     * 每页显示行数
     * @date 2023/2/23 10:58
    */
    private long pageSize;

    /**
     * 总行数
     * @date 2023/2/23 10:58
    */
    private long totalSize;

    /**
     * 页码总数
     * @date 2023/2/23 10:58
    */
    private long totalPages;

    /**
     * 数据模型
     * @date 2023/2/23 10:59
    */
    private List<T> contentData;

    /**
     * 是否为第一页
     */
    private boolean isFirstPage = false;

    /**
     * 是否为最后一页
     */
    private boolean isLastPage = false;

    /**
     * 是否有前一页
     */
    private boolean hasPreviousPage = false;

    /**
     * 是否有下一页
     * @date 2024/7/26 下午5:03
     */
    private boolean hasNextPage = false;

    public long getPageNum() {
        return pageNum;
    }

    public void setPageNum(long pageNum) {
        this.pageNum = pageNum;
    }

    public long getPageSize() {
        return pageSize;
    }

    public void setPageSize(long pageSize) {
        this.pageSize = pageSize;
    }

    public long getTotalSize() {
        return totalSize;
    }

    public void setTotalSize(long totalSize) {
        this.totalSize = totalSize;
    }

    public long getTotalPages() {
        return totalPages;
    }

    public void setTotalPages(long totalPages) {
        this.totalPages = totalPages;
    }

    public List<T> getContentData() {
        return contentData;
    }

    public void setContentData(List<T> contentData) {
        this.contentData = contentData;
    }

    public boolean isFirstPage() {
        return isFirstPage;
    }

    public void setFirstPage(boolean firstPage) {
        isFirstPage = firstPage;
    }

    public boolean isLastPage() {
        return isLastPage;
    }

    public void setLastPage(boolean lastPage) {
        isLastPage = lastPage;
    }

    public boolean isHasPreviousPage() {
        return hasPreviousPage;
    }

    public void setHasPreviousPage(boolean hasPreviousPage) {
        this.hasPreviousPage = hasPreviousPage;
    }

    public boolean isHasNextPage() {
        return hasNextPage;
    }

    public void setHasNextPage(boolean hasNextPage) {
        this.hasNextPage = hasNextPage;
    }

    @Override
    public boolean equals(Object object) {

        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;
        PageResult<?> that = (PageResult<?>) object;
        return pageNum == that.pageNum && pageSize == that.pageSize && totalSize == that.totalSize && totalPages == that.totalPages && isFirstPage == that.isFirstPage && isLastPage == that.isLastPage && hasPreviousPage == that.hasPreviousPage && hasNextPage == that.hasNextPage && Objects.equals(contentData, that.contentData);
    }

    @Override
    public int hashCode() {
        return Objects.hash(pageNum, pageSize, totalSize, totalPages, contentData, isFirstPage, isLastPage, hasPreviousPage, hasNextPage);
    }

    @Override
    public String toString() {
        return "{" +
                "\"pageNum\": " + pageNum +","+
                "\"pageSize\": " + pageSize +","+
                "\"totalSize\": " + totalSize +","+
                "\"totalPages\": " + totalPages +","+
                "\"isFirstPage\": " + isFirstPage +","+
                "\"isLastPage\": " + isLastPage +","+
                "\"hasPreviousPage\": " + hasPreviousPage +","+
                "\"hasNextPage\": " + hasNextPage +","+
                "\"contentData\": " + contentData +
                '}';
    }

    public PageResult(long pageNum, long pageSize, long totalSize, long totalPages, List<T> contentData) {
        this.pageNum = pageNum;
        this.pageSize = pageSize;
        this.totalSize = totalSize;
        this.totalPages = totalPages;
        this.contentData = contentData;
        this.isFirstPage = pageNum == 1;
        this.isLastPage = pageNum == totalPages || totalPages == 0;
        this.hasPreviousPage = pageNum > 1;
        this.hasNextPage = pageNum < totalPages;
    }

    public PageResult() {
    }
}
