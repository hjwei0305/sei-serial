package com.changhong.sei.serial.api;

import com.changhong.sei.core.dto.ResultData;
import com.changhong.sei.serial.dto.BarCodeAssociateDto;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

/**
 * <strong>实现功能:</strong>
 * <p>条码查询接口</p>
 *
 * @author 刘松林
 * @version 1.0.1 2020-02-21 8:27
 */
@FeignClient(value = "sei-serial",path = "serialNumberConfig")
public interface SerialNumberConfigApi {

    @GetMapping("getReferenceIdByBarCode")
    @ApiOperation(value = "通过条码查找对应的关联关系", notes = "通过条码查找对应的关联关系")
    ResultData<BarCodeAssociateDto> getReferenceIdByBarCode(@RequestParam String barCode);

    @GetMapping("getBarCodeListByReferenceId")
    @ApiOperation(value = "通过关联id找对应的条码", notes = "通过关联id找对应的条码")
    ResultData<List<BarCodeAssociateDto>> getBarCodeListByReferenceId(@RequestParam String referenceId);

    @PostMapping("getReferenceIdsByListBarCode")
    @ApiOperation(value = "通过条码列表查找对应的关联关系", notes = "通过条码列表查找对应的关联关系")
    ResultData<List<BarCodeAssociateDto>> getReferenceIdsByListBarCode(@RequestBody List<String> barCodes);

}
