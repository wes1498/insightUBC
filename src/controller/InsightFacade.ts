import Log from "../Util";
import {IInsightFacade, InsightDataset, InsightDatasetKind, InsightCourse} from "./IInsightFacade";
import {InsightError, NotFoundError} from "./IInsightFacade";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */

const COURSES = InsightDatasetKind.Courses;
const ROOMS = InsightDatasetKind.Rooms;
const coursesMap: Map<string, InsightCourse[]> = new Map<string, InsightCourse[]>();
export default class InsightFacade implements IInsightFacade {

    constructor() {
        Log.trace("InsightFacadeImpl::init()");
    }

    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        return Promise.reject("Not implemented.");
    }

    public removeDataset(id: string): Promise<string> {
        return Promise.reject("Not implemented.");
    }

    public performQuery(query: any): Promise <any[]> {
        return Promise.reject("Not implemented.");
    }

    public listDatasets(): Promise<InsightDataset[]> {
                return new Promise<InsightDataset[]>((fulfill, reject) => {
                        const result: InsightDataset[] = [];
                        for (const id of coursesMap.keys()) {
                                const numRows = coursesMap.get(id).length;
                                result.push({id, kind: COURSES, numRows});
                        }
                        const body: InsightDataset[] = result;
                        fulfill();
        });
    }
}
