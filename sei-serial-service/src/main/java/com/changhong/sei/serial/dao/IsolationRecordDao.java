package com.changhong.sei.serial.dao;

import com.changhong.sei.core.dao.BaseEntityDao;
import com.changhong.sei.serial.entity.IsolationRecord;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;


/***
 * 隔离记录，每个类配置都可以进行灵活隔离
 */
@Repository
public interface IsolationRecordDao extends BaseEntityDao<IsolationRecord> {

    @Query("select isr from IsolationRecord isr where isr.configId = :configId and isr.isolationCode = :isolation and isr.dateString = :dateString")
    IsolationRecord findByConfigIdAndIsolationCodeAndDateString(@Param("configId") String configId,
                                                                @Param("isolation") String isolation,
                                                                @Param("dateString")String dateString);

    @Modifying
    @Transactional
    @Query("update IsolationRecord isr set isr.currentNumber = :currentNumber where id = :id")
    void updateCurrentSerial(@Param("id") String id,@Param("currentNumber") Long currentNumber);
}
