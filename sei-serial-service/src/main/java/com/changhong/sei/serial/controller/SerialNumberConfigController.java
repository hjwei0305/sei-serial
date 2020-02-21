package com.changhong.sei.serial.controller;

import com.changhong.sei.core.dto.ResultData;
import com.changhong.sei.core.dto.serach.PageResult;
import com.changhong.sei.core.dto.serach.Search;
import com.changhong.sei.core.dto.serach.SearchFilter;
import com.changhong.sei.core.service.bo.OperateResultWithData;
import com.changhong.sei.serial.entity.BarCodeAssociate;
import com.changhong.sei.serial.entity.SerialNumberConfig;
import com.changhong.sei.serial.entity.dto.BarCodeDto;
import com.changhong.sei.serial.entity.enumclass.ConfigType;
import com.changhong.sei.serial.sdk.BarCodeService;
import com.changhong.sei.serial.service.BarCodeAssociateService;
import com.changhong.sei.serial.service.SerialNumberConfigService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("serialNumberConfig")
public class SerialNumberConfigController {

    @Autowired
    private SerialNumberConfigService serialNumberConfigService;

    @Autowired
    private BarCodeAssociateService barCodeAssociateService;

    @PostMapping("save")
    public ResultData<SerialNumberConfig> save(@RequestBody SerialNumberConfig serialNumberConfig){
        OperateResultWithData<SerialNumberConfig> result = serialNumberConfigService.save(serialNumberConfig);
        return ResultData.success(result.getData());
    }

    @PostMapping("findAll")
    public ResultData<PageResult> findAll(@RequestBody Search searchConfig){
        PageResult<SerialNumberConfig> result = serialNumberConfigService.findByPage(searchConfig);
        return ResultData.success(result);
    }

    @PostMapping("delete/{id}")
    public ResultData<?> delete(@PathVariable("id") String id){
        serialNumberConfigService.delete(id);
        return ResultData.success(true);
    }

    @GetMapping("findByClassName")
    public SerialNumberConfig findByClassName(@RequestParam String className){
        return serialNumberConfigService.findByClassNameAndConfigType(className, ConfigType.CODE_TYPE);
    }

    @PostMapping("genAndSaveAssociate")
    public String genNumberAndSaveAssociate(@RequestBody BarCodeDto barCodeDto){
      return serialNumberConfigService.genNumberAndSaveAssociate(barCodeDto);
    }

    @GetMapping("getReferenceIdByBarCode")
    public ResultData<BarCodeAssociate> getReferenceIdByBarCode(@RequestParam String barCode){
        BarCodeAssociate barCodeAssociate = barCodeAssociateService.findByProperty("barCode",barCode);
        return ResultData.success(barCodeAssociate);
    }

    @GetMapping("getBarCodeListByReferenceId")
    public ResultData<List<BarCodeAssociate>> getBarCodeListByReferenceId(@RequestParam String referenceId){
        List<BarCodeAssociate> barCodeAssociates = barCodeAssociateService.findListByProperty("referenceId",referenceId);
        return ResultData.success(barCodeAssociates);
    }

    @GetMapping("getReferenceIdsByListBarCode")
    public ResultData<List<BarCodeAssociate>> getReferenceIdsByListBarCode(@RequestBody List<String> barCodes){
        SearchFilter searchFilter = new SearchFilter();
        searchFilter.setFieldName("barCode");
        searchFilter.setOperator(SearchFilter.Operator.IN);
        searchFilter.setValue(barCodes);
        List<BarCodeAssociate> barCodeAssociates = barCodeAssociateService.findByFilter(searchFilter);
        return ResultData.success(barCodeAssociates);
    }




}