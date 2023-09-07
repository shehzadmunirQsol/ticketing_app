import aws from 'aws-sdk';

export default function awsConfig() {
  const opt = {
    accessKeyId: process.env?.AWS_ACCESS_KEY,
    secretAccessKey: process.env?.AWS_SECRET_KEY,
    region: process.env?.AWS_REGION,
    // endpoint: "http://localhost:8000",
  };

  aws.config.update(opt);
  // this.client = new aws.DynamoDB.DocumentClient();
}
