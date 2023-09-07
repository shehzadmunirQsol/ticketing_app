import mainRoutes from '../../server/routes';
import awsConfig from '~/server/lib/AWSConfig';
import { NextApiRequest, NextApiResponse } from 'next';

//# AWS Configuration
awsConfig();

//# DynamoDB Connection
// const db = new dynamoDBConnect();

//# MongoDB Connection
// mongoDBConnect();

// The following should be exported from your API route file

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse,
) {
  return mainRoutes(req, res);
}
