import Log from "../Util";
import {
    IInsightFacade,
    InsightDataset,
    InsightDatasetKind,
    InsightError,
    InsightFilter,
    NotFoundError
} from "./IInsightFacade";
import * as JSZip from "jszip";
import {JSZipObject} from "jszip";
import * as fs from "fs";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */

export default class InsightFacade implements IInsightFacade {
    private coursesMap: Map<string, any>;
    constructor() {
        Log.trace("InsightFacadeImpl::init()");
        this.coursesMap = new Map<string, any>();
    }

    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        let that = this;
        return new Promise<string[]>(function (resolve, reject) {
            if (typeof id !== "string" || typeof content !== "string" || kind === undefined || kind === null) {
                reject(new InsightError("Invalid params"));
            }
            if (that.coursesMap.has(id)) {
                return reject(new InsightError("Id is already added"));
            }
            if (!id || id.length === 0 || id === "") {
                return reject(new InsightError("Invalid Id"));
            }
            that.coursesMap.set(id, []);
            JSZip.loadAsync(content, {base64: true}).then((unzipped: JSZip) => {

                let filesPromise: any[] = [];
                if (unzipped.files.hasOwnProperty("courses/") || unzipped.length > 0) {
                    unzipped.forEach((function (relativePath, fileObject: JSZipObject) {

                        filesPromise.push(fileObject.async("text").then((data: string) => {
                            try {
                                let parsedInfo = JSON.parse(data);
                                if ((parsedInfo.result.length === 0)) {
                                    throw new Error("None to store");
                                }
                                parsedInfo.result.forEach(function (section: any) {
                                    let dept: string = section.Subject;
                                    let iid: string = section.Course;
                                    let avg: number = section.Avg;
                                    let instructor: string = section.Professor;
                                    let title: string = section.Title;
                                    let pass: number = section.Pass;
                                    let fail: number = section.Fail;
                                    let audit: number = section.Audit;
                                    let uuid: string = section.id.toString();
                                    let year: number = Number(section.Year);

                                    // let validCourse: CourseSaver = new CourseSaver(dept, id,
                                    //     avg, instructor, title, pass, fail, audit, uuid, year);
                                    let validCourse: any = {
                                        dept: String,
                                        id: String,
                                        avg: Number,
                                        instructor: String,
                                        title: String,
                                        pass: Number,
                                        fail: Number,
                                        audit: Number,
                                        uuid: String,
                                        year: Number
                                    };
                                    validCourse.dept = dept;
                                    validCourse.id = iid;
                                    validCourse.avg = avg;
                                    validCourse.instructor = instructor;
                                    validCourse.title = title;
                                    validCourse.pass = pass;
                                    validCourse.fail = fail;
                                    validCourse.audit = audit;
                                    validCourse.uuid = uuid;
                                    validCourse.year = year;
                                    that.coursesMap.get(id).push(validCourse);
                                });

                            } catch (e) {
                                // not in JSON format or some fields of different type/missing-> skip this course
                            }
                        }).catch((e) => {
                            return reject(new InsightError("Error in adding dataset " + e));
                        }));
                    }));
                } else {
                    return reject(new InsightError("Desired folder for the dataset kind does not exist"));
                }
                Promise.all(filesPromise).then(function () {
                    let toSaveOnDisk: object[] = that.coursesMap.get(id);
                    // if (toSaveOnDisk.length === 0) {
                    //     that.coursesMap.delete(id);
                    //     return reject(new InsightError("No sections were added"));
                    // } else {
                    fs.writeFile("data/" + id + ".json", JSON.stringify(toSaveOnDisk), function (e) {
                            return reject(new InsightError("Error Saving Files " + e));
                        });

                    let result: string[] = [];
                    that.coursesMap.forEach(function (value, key) {
                            result.push(key);
                        });
                    return resolve(result);
                    // }
                });
            }).catch((e) => {
                return reject(new InsightError("Error decoding contents: Invalid Zip " + e));
            });
        });
    }

    public removeDataset(id: string): Promise<string> {
        let that = this;
        return new Promise<string>(function (resolve, reject) {
            if (id === "") {
                return reject(new NotFoundError("Invalid ID"));
            } else if (id === null || !id) {
                return reject(new InsightError("Invalid ID"));
            } else if (!that.coursesMap.has(id)) {
                return reject(new NotFoundError("Id not in Map"));
            }
            if (that.coursesMap.has(id)) {
                fs.readdir(InsightDatasetKind.Courses.toString(), function (e, files) {
                    if (e) {
                        return reject(new InsightError("Dataset not added yet " + e));
                    }
                });
            }

            that.coursesMap.delete(id);
            return resolve (id);
        });
    }
    public performQuery(query: any): Promise<any[]> {
        let that = this;
        return new Promise<any[]>(function (resolve, reject) {
            try {
                let filter: InsightFilter = query.WHERE;
                let options = query.OPTIONS;
                let order = options.ORDER;
                let columns = options.COLUMNS;
                let id: string = columns[0].split("_")[0];
                if (that.coursesMap.get(id) === undefined) {
                        reject(new InsightError("noot"));
                }
                // let sections = fs.readFileSync("./data/" + id + ".json", "UTF8");
                // if (!sections) {
                //     reject(new InsightError("id does not exist"));
                // }
                // let parsedInfo = JSON.parse(sections);
                let result: any[];
                if (Object.keys(filter).length === 0) {
                    result = that.coursesMap.get(id);

                    if (result.length > 5000) {
                        throw new InsightError("Too many sections in result");
                    }
                } else {
                    let thisResult: any[] = [];

                    for (let section of that.coursesMap.get(id)) {
                        // check if you can apply filter to the key
                        if (InsightFacade.isSectionValid(filter, section, id)) {
                            thisResult.push(section);
                        }
                    }
                    // result over 5000 to add
                    if (thisResult.length > 5000) {
                        throw new InsightError("Result exceeds 5000 limit");
                    }
                    result = thisResult;
                }
                // keep only the desired columns in query
                if (columns && columns.length !== 0) {
                    let columnResult: object[] = [];
                    result.forEach( function (section: any) {
                        let columnSection: any = {};
                        columns.forEach( function (key: any) {
                            if (InsightFacade.validKeyHelper(key, id)) {
                                let res = key.substring(0, key.indexOf("_"));
                                key = key.substring(key.indexOf("_") + 1);
                                columnSection[res + "_" + key] = section[key];
                            }
                        });
                        columnResult.push(columnSection);
                    });
                    result = columnResult;
                    // result = this.desiredColumnsHelper(result, columns);
                }
                // Sort the result if order is included
                if (order !== undefined || order !== null) {
                    if (columns.includes(order)) {
                        result = result.sort( function (a, b) {
                            let x = a[order];
                            let y = b[order];
                            if (x === y) {
                                return 0;
                            } else if (x > y ) {
                                return 1;
                            } else {
                                return -1;
                            }
                        });
                    } else {
                        throw new InsightError("ORDER not in COLUMNS");
                    }
                }
                // resolve if no problems
                return resolve(result);
            } catch (e) {
                return reject(new InsightError("Error in reading query"));
            }

        });
    }

    private static validKeyHelper(key: string, id: string): boolean {
        // check if the key being passed is a valid one
        switch (key) {
            case id + "_dept":
                return true;
            case id + "_id":
                return true;
            case id + "_instructor":
                return true;
            case id + "_title":
                return true;
            case id + "_uuid":
                return true;
            case id + "_avg":
                return true;
            case id + "_pass":
                return true;
            case id + "_fail":
                return true;
            case id + "_audit":
                return true;
            case id + "_year":
                return true;
            default:
                return false;
        }
    }

    // Check if filter applies to given section
    private static isSectionValid(filter: InsightFilter, section: any, id: string): boolean {
        if (filter.hasOwnProperty("AND")) {
            if (filter.AND.length === 0 ) {
                throw new InsightError("Not enough conditions for AND");
            }
                // for each filter section must be valid
                // recursive call to each filter in the array on the section
            for (let insideFilter of filter.AND) {
                if (!this.isSectionValid(insideFilter, section, id)) {
                    return false;
                }
            }
                // if all filters valid on each section
            return true;

        }
        if (filter.hasOwnProperty("OR")) {
            // OR must be 1 or more
            if (filter.OR.length === 0) {
                throw new InsightError("Not enough conditions for OR");
            }
                // for at least one filter section must be valid:
                // recursive call to each filter in the array on the section
            for (let insideFilter of filter.OR) {
                if (this.isSectionValid(insideFilter, section, id)) {
                    return true;
                }
            }
            return false;
        }
            // check if it is an MCOMPARATOR
        if (filter.hasOwnProperty("GT") || filter.hasOwnProperty("LT") || filter.hasOwnProperty("EQ")) {
            // this.validateMComFilterHelper(filter);
            let body = Object.values(filter)[0];
            // MCOMPARATOR must be a number
            // check for valid value
            if (typeof Object.values(body)[0] !== "number") {
                throw new InsightError("Invalid value");
            }
            // check for valid key
            if (!this.validKeyHelper(Object.keys(body)[0], id)) {
                throw new InsightError("Invalid key");
            }

            if (filter.hasOwnProperty("GT")) {
                let yup = Object.keys(filter.GT)[0]; // courses_avg
                yup = yup.substring(yup.indexOf("_") + 1); // avg

                if (section[yup] > Object.values(filter.GT)[0]) { // section[avg]
                    return true;
                } else {
                    return false;
                }

            } else if (filter.hasOwnProperty("LT")) {
                let yup = Object.keys(filter.LT)[0];
                yup = yup.substring(yup.indexOf("_") + 1);

                if (section[yup] < Object.values(filter.LT)[0]) {
                    return true;
                } else {
                    return false;
                }

            } else {
                let yup = Object.keys(filter.EQ)[0];
                yup = yup.substring(yup.indexOf("_") + 1);

                if (section[ yup] === Object.values(filter.EQ)[0]) {

                    return true;
                } else {
                    return false;
                }

            }
        } else if (filter.hasOwnProperty("NOT")) {

            // recursively call the same function with the same inputs but negated
            return (!this.isSectionValid(filter.NOT, section, id));

            // Check if it is an SComparison
        } else if (filter.hasOwnProperty("IS")) {

            // return InsightFacade.handleSComparisonHelper(filter.IS, section);

            let key: any = Object.keys(filter.IS)[0]; // courses_id
            // check if key is not invalid
            if (!this.validKeyHelper(key, id)) {
                throw new InsightError("Invalid key");
            }
            let value: any = Object.values(filter.IS)[0]; // courses_avg: VALUE
            // check if value is of right type
            if (typeof value !== "string") {
                throw new InsightError("Invalid type");
            }
            let actualRes: string = section[key.substring(key.indexOf("_") + 1)]; // section[id]
            // check each wildcard case
            if (value.includes("*")) {
                if (value.length === 1) {
                    return value === "*";
                    // *ell*, *ello, hell*
                } else if (value.length === 2 && value.startsWith("**")) {
                    return value === "**";
                } else if (value.startsWith("**") || value.endsWith("**")) {
                    throw new InsightError("Asteriks cannot be in the middle");
                } else if (value.startsWith("*") && value.endsWith("**")) {
                    throw new InsightError("Asteriks cannot be in the middle");
                } else if (value.startsWith("**") && value.endsWith("*")) {
                    throw new InsightError("Asteriks cannot be in the middle");
                } else if (value.startsWith("*") && value.endsWith("*")) {
                    return actualRes.includes(value.substring(1, value.length - 1));
                } else if (value.startsWith("*")) {
                    return actualRes.endsWith(value.substring(1));
                } else if (value.endsWith("*")) {
                    return actualRes.startsWith(value.substring(0, value.length - 1));
                } else {
                    // h**lo or h*llo === error
                    throw new InsightError("Asteriks cannot be in the middle");
                }
            }
            return value === actualRes;
        } else {
            // if no there is no filter
            if (filter.constructor === Object && Object.keys(filter).length === 0) {
                return false;
            } else {
                throw new InsightError("Did not match any of the keys");
            }
        }
    }
    public listDatasets(): Promise<InsightDataset[]> {
        let that = this;
        return new Promise<InsightDataset[]> ( function (resolve, reject) {
            let result: InsightDataset[] = [];

            for (let id of that.coursesMap.keys()) {
                let crows: number = that.coursesMap.get(id).length;
                result.push({id, kind: InsightDatasetKind.Courses, numRows: crows});
            }

            resolve(result);
        });
    }
}
