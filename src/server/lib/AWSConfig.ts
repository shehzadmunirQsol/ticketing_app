import aws from 'aws-sdk';

export default function awsConfig() {
  const opt = {
    accessKeyId: process.env?.ACCESS_KEY,
    secretAccessKey: process.env?.SECRET_KEY,
    region: process.env?.REGION,
    // endpoint: "http://localhost:8000",
  };

  aws.config.update(opt);
  // this.client = new aws.DynamoDB.DocumentClient();
}
