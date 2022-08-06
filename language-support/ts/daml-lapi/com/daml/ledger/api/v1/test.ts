import { Value, Variant, Record, Enum } from "./value_pb";

const decodeVariant = (decoders, v) => {
    const actualConstr = v.getConstructor();
    const decoder = decoders.find(({constr}) => constr === actualConstr);
    if (!decoder) {
        throw new Error(`New decoder for constructor ${actualConstr}`);
    }
    return {tag: actualConstr, value: decoder.decode(v.getValue()!)};
};

const decodeRecord = (decoders, r) => {
    const fields = r.getFieldsList();
    let obj = {};
    fields.forEach(field => {
        const decoder = decoders[field.getLabel()];
        obj[field.getLabel()] = decoder(field.getValue()!);
    });
    return obj;
};


const decodeEnum = (e) => e.getConstructor();

